
module Main (main) where

import Build
import HackageTestMonad
import Utils

import Control.Monad.State
import Data.List
import Distribution.Package
import Distribution.Text
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = evalStateT main' startState

main' :: M ()
main' = do liftIO $ hSetBuffering stdout NoBuffering
           args <- liftIO getArgs
           case args of
               name : cabalInstall : ghc : ghcPkg : depFlags : pkgFlags : ps ->
                   do exists <- liftIO $ doesDirectoryExist name
                      when exists $ die (show name ++ " already exists")
                      liftIO $ createDirectory name
                      liftIO $ createDirectory (name </> "logs.stats")
                      liftIO $ createDirectory (name </> "logs.build")
                      setName name
                      setCabalInstall cabalInstall
                      setGhc ghc
                      setGhcPkg ghcPkg
                      setDepFlags depFlags
                      setPkgFlags pkgFlags
                      ps' <- case ps of
                             [] -> getPackages
                             _  -> return ps
                      tryBuildingPackages ps'
               ["help"] ->
                   liftIO usageInfo
               ["--help"] ->
                   liftIO usageInfo
               _ ->
                   liftIO $ do usageInfo
                               exitWith (ExitFailure 1)

usageInfo :: IO ()
usageInfo = do prog <- getProgName
               mapM_ putStrLn ["Usage: " ++ prog ++ " name cabalInstall ghc ghcPkg depFlags pkgFlags",
                               "    name:         A name by which the results of this hackage test run will",
                               "                  by referred, e.g. \"ghc-6.12.1\".",
                               "    cabalInstall: The path to the cabal-install program to use.",
                               "    ghc:          The path to the ghc program to use.",
                               "    ghcPkg:       The path to the ghc-pkg program to use.",
                               "    depFlags:     The flags to use when compiling dependencies of a package",
                               "                  we are interested in, e.g. \"\" or \"-XFoo -XBar\".",
                               "    pkgFlags:     The flags to use when compiling a package we are interested",
                               "                  in, e.g. \"\" or \"-XFoo -XBar\"."]

tryBuildingPackages :: [PkgName] -> M ()
tryBuildingPackages ps
 = do setNumPackages (length ps)
      -- Our main objective here is to find out how many times each
      -- package would be installed as a dependency of another package.
      zipWithM_ doPackageStats ps [1..]
      liftIO $ putStrLn ""
      liftIO $ putStrLn ""
      -- Write out the data so we can look at it (by hand) later if we
      -- want.
      dumpStats
      unless statsOnly $ do
          commonPackageConf <- getCommonPackageConf
          initialisePackageConf commonPackageConf
          if buildCommonPackagesOnce
              then do liftIO $ putStrLn "Testing common dependency packages"
                      psCommon <- getCommonDepInstallablePackages
                      zipWithM_ (doPackageBuild (length psCommon)) psCommon [1..]
                      liftIO $ putStrLn "Building common dependency packages"
                      buildInCommon psCommon
                      liftIO $ putStrLn "Testing remaining dependency packages"
                      psAll <- getInstallablePackages
                      let psUncommon = psAll \\ psCommon
                      zipWithM_ (doPackageBuild (length psUncommon)) psUncommon [1..]
              else do psAll <- getInstallablePackages
                      zipWithM_ (doPackageBuild (length psAll)) psAll [1..]
          dumpResults

-- The build is faster if we only build common dependencies once, but
-- currently the code assumes that all of them will install. As this
-- isn't the case, turn it off.
buildCommonPackagesOnce :: Bool
buildCommonPackagesOnce = False

statsOnly :: Bool
statsOnly = False

getPackages :: M [PkgName]
getPackages = do cabalInstall <- getCabalInstall
                 m <- runCmdGetStdout cabalInstall ["list", "--simple-output", "-v0"]
                 case m of
                     Nothing -> die "Failed to get package list"
                     Just xs ->
                         do let ls = lines xs
                                ls' = map (takeWhile (' ' /=)) ls
                                ps = uniq $ filter (not . null) ls'
                            return ps

-- Find out what would happen if we installed the package.
-- Running cabal-install in dry-run mode may tell us:
-- * The package is uninstallable
-- * The package is already installed
-- * The package is installable, and which other packages we would have
--   to install in order to install it
-- * Something we don't understand
doPackageStats :: PkgName -> Int -> M ()
doPackageStats p n
    = do st <- get
         liftIO $ putStr ("\r" ++ show n ++ " of " ++ show (st_numPackages st))
         cabalInstall <- getCabalInstall
         basicFlags <- getBasicCabalInstallFlags
         -- It's not clear if we want getDepFlags or getPkgFlags here,
         -- but it probably doesn't matter anyway
         fs <- getDepFlags
         name <- getName
         let summaryName = name </> "logs.stats" </> p <.> "summary"
             logName     = name </> "logs.stats" </> p <.> "log"
             resultName  = name </> "logs.stats" </> p <.> "result"
             args = basicFlags ++ fs ++
                    ["install", "--dry-run", p,
                     "--build-summary=" ++ summaryName,
                     "--build-log=" ++ logName]
         e <- runCmdGetResults cabalInstall args
         case e of
             Left (ec, ls) ->
                 do -- Ideally cabal-install would have written out some
                    -- sort of log, but it seems not to do so in dry-run
                    -- mode, so we do so ourselves
                    let resultsLines = "FAILED"
                                     : ("Exit code: " ++ show ec)
                                     : map mkResultLine ls
                        mkResultLine (Stdout l) = "Stdout: " ++ l
                        mkResultLine (Stderr l) = "Stderr: " ++ l
                    liftIO $ writeFile resultName $ unlines resultsLines
                    addNotInstallablePackage p
             Right ls ->
                 do -- Ideally cabal-install would have written out some
                    -- sort of log, but it seems not to do so in dry-run
                    -- mode, so we do so ourselves
                    let resultsLines = "SUCCEEDED" : ls
                    liftIO $ writeFile resultName $ unlines resultsLines
                    case break (listHeaderPrefix `isPrefixOf`) ls of
                        (_, _ : ls1) ->
                            do ls2 <- mapM mangleLine ls1
                               case reverse ls2 of
                                   thisP : otherPs
                                    | thisP == p ->
                                       do addInstallablePackage p
                                          mapM_ addInstall otherPs
                                   _ ->
                                       die ("Installing " ++ show p ++
                                            " doesn't end by installing it")
                        _
                         | any (noPackagesPrefix `isPrefixOf`) ls ->
                            addInstalledPackage p
                         | otherwise ->
                            do warn ("Don't understand result for " ++ show p)
                               addFailPackage p
    where listHeaderPrefix = "In order, the following would be installed"
          noPackagesPrefix = "No packages to be installed."
          mangleLine l = case simpleParse l of
                         Nothing ->
                             die ("Unparseable line: " ++ show l)
                         Just (PackageIdentifier (PackageName pn) _) ->
                             return pn

