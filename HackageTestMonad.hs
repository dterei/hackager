
module HackageTestMonad where

import Utils

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (catch)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

type PkgName = String
type M = StateT St IO
-- We're a bit sloppy with this type. Different fields only get good
-- values at different phases.
data St = St {
              -- These are set based on the command line flags
              st_name :: FilePath,
              st_dir :: FilePath,
              st_cabalInstall :: FilePath,
              st_ghc :: FilePath,
              st_ghcPkg :: FilePath,
              st_depFlags :: [String],
              st_pkgFlags :: [String],
              -- This is set immediately after we get the list of packages:
              st_numPackages :: Int,
              -- These are set by the stats-collection pass:
              st_installedPackages :: Set PkgName,
              st_installablePackages :: Set PkgName,
              st_notInstallablePackages :: Set PkgName,
              st_failPackages :: Set PkgName,
              st_installCounts :: Map PkgName Int,
              -- These are set by the installation pass:
              st_buildablePackages :: Set PkgName,
              st_buildFailurePackages :: Set PkgName,
              st_buildDepFailurePackages :: Set PkgName
          }

startState :: St
startState = St {
                 st_name = "",
                 st_dir = "",
                 st_cabalInstall = "",
                 st_ghc = "",
                 st_ghcPkg = "",
                 st_depFlags = [],
                 st_pkgFlags = [],
                 st_numPackages = 0,
                 st_installedPackages = Set.empty,
                 st_installablePackages = Set.empty,
                 st_notInstallablePackages = Set.empty,
                 st_failPackages = Set.empty,
                 st_installCounts = Map.empty,
                 st_buildablePackages = Set.empty,
                 st_buildFailurePackages = Set.empty,
                 st_buildDepFailurePackages = Set.empty
             }

------------------------------------------------
-- Helpers

setName :: FilePath -> M ()
setName name = do st <- get
                  dir <- liftIO getCurrentDirectory
                  put $ st { st_name = name, st_dir = dir </> name }

getName :: M FilePath
getName = do st <- get
             return $ st_name st

getDir :: M FilePath
getDir = do st <- get
            return $ st_dir st

getCommonPackageConf :: M FilePath
getCommonPackageConf = do dir <- getDir
                          return (dir </> "common.package.conf")

getCommonPrefix :: M FilePath
getCommonPrefix = do dir <- getDir
                     return (dir </> "commonPrefix")

getTempPackageConf :: M FilePath
getTempPackageConf = do dir <- getDir
                        return (dir </> "temp.package.conf")

getScratchDir :: M FilePath
getScratchDir = do dir <- getDir
                   return (dir </> "scratch")

setCabalInstall :: FilePath -> M ()
setCabalInstall cabalInstall = do st <- get
                                  put $ st { st_cabalInstall = cabalInstall }

getCabalInstall :: M FilePath
getCabalInstall = do st <- get
                     return $ st_cabalInstall st

setGhc :: FilePath -> M ()
setGhc ghc = do st <- get
                put $ st { st_ghc = ghc }

getGhc :: M FilePath
getGhc = do st <- get
            return $ st_ghc st

setGhcPkg :: FilePath -> M ()
setGhcPkg ghcPkg = do st <- get
                      put $ st { st_ghcPkg = ghcPkg }

getGhcPkg :: M FilePath
getGhcPkg = do st <- get
               return $ st_ghcPkg st

setDepFlags :: String -> M ()
setDepFlags depFlags = do st <- get
                          put $ st { st_depFlags = parseFlags depFlags }

getDepFlags :: M [String]
getDepFlags = do st <- get
                 return $ st_depFlags st

setPkgFlags :: String -> M ()
setPkgFlags pkgFlags = do st <- get
                          put $ st { st_pkgFlags = parseFlags pkgFlags }

getPkgFlags :: M [String]
getPkgFlags = do st <- get
                 return $ st_pkgFlags st

parseFlags :: String -> [String]
parseFlags str = case reads str of
                 [(flags, "")] -> flags
                 _ -> words str

setNumPackages :: Int -> M ()
setNumPackages n = do st <- get
                      put $ st { st_numPackages = n }

addInstall :: PkgName -> M ()
addInstall pn = do st <- get
                   let ics = st_installCounts st
                       ics' = Map.insertWith (+) pn 1 ics
                   put $ st { st_installCounts = ics' }

addInstalledPackage :: PkgName -> M ()
addInstalledPackage pkg
 = do st <- get
      let s = st_installedPackages st
      put $ st { st_installedPackages = Set.insert pkg s }

addInstallablePackage :: PkgName -> M ()
addInstallablePackage pkg
 = do st <- get
      let s = st_installablePackages st
      put $ st { st_installablePackages = Set.insert pkg s }

getInstallablePackages :: M [PkgName]
getInstallablePackages
 = do st <- get
      return $ Set.toList $ st_installablePackages st

getCommonDepInstallablePackages :: M [PkgName]
getCommonDepInstallablePackages
 = do st <- get
      let isInstallable p = p `Set.member` st_installablePackages st
          commonDeps = Map.keys $ Map.filter (> 50) $ st_installCounts st
          commonDepsInstallable = filter isInstallable commonDeps
      return commonDepsInstallable

addNotInstallablePackage :: PkgName -> M ()
addNotInstallablePackage pkg
 = do st <- get
      let s = st_notInstallablePackages st
      put $ st { st_notInstallablePackages = Set.insert pkg s }

addFailPackage :: PkgName -> M ()
addFailPackage pkg
 = do st <- get
      let s = st_failPackages st
      put $ st { st_failPackages = Set.insert pkg s }

buildSucceeded :: PkgName -> M ()
buildSucceeded pkg
 = do st <- get
      let s = st_buildablePackages st
      put $ st { st_buildablePackages = Set.insert pkg s }

buildFailed :: PkgName -> M ()
buildFailed pkg
 = do st <- get
      let s = st_buildFailurePackages st
      put $ st { st_buildFailurePackages = Set.insert pkg s }

buildDepsFailed :: PkgName -> M ()
buildDepsFailed pkg
 = do st <- get
      let s = st_buildDepFailurePackages st
      put $ st { st_buildDepFailurePackages = Set.insert pkg s }

dumpStats :: M ()
dumpStats
    = do st <- get
         let fullHistogram = reverse $ sort $ map swap
                           $ Map.assocs $ st_installCounts st
             (manyHistogram, fewHistogram) = span ((>= 10) . fst) fullHistogram
             total = sum $ map fst fullHistogram
             summaryTable = [["Num packages",
                              show $ st_numPackages st],
                             ["Installed packages",
                              show $ Set.size $ st_installedPackages st],
                             ["Installable packages",
                              show $ Set.size $ st_installablePackages st],
                             ["Not installable packages",
                              show $ Set.size $ st_notInstallablePackages st],
                             ["Failed packages",
                              show $ Set.size $ st_failPackages st],
                             ["Total reinstallations",
                              show total]]

         name <- getName
         liftIO $ do
             writeFile (name </> "stats.full")
                       (unlines $ showCompleteHistogram fullHistogram)
             writeFile (name </> "stats.many")
                       (unlines $ showCompleteHistogram manyHistogram)
             writeFile (name </> "stats.few")
                       (unlines $ showSummaryHistogram fewHistogram)
             writeFile (name </> "stats.summary")
                       (unlines $ showTable [rpad, rpad] summaryTable)
             writeFile (name </> "installed-packages")
                       (unlines $ Set.toList
                                $ st_installedPackages st)
             writeFile (name </> "installable-packages")
                       (unlines $ Set.toList
                                $ st_installablePackages st)
             writeFile (name </> "uninstallable-packages")
                       (unlines $ Set.toList
                                $ st_notInstallablePackages st)
             writeFile (name </> "fail-packages")
                       (unlines $ Set.toList
                                $ st_failPackages st)
             writeFile (name </> "install-counts")
                       (unlines $ map show
                                $ Map.assocs
                                $ st_installCounts st)
    where showCompleteHistogram hist = showTable [rpad, rpad]
                                                 [ [show count, pkg]
                                                 | (count, pkg) <- hist ]
          showSummaryHistogram hist
              = let hist' = groupBy (on (==) fst) hist
                    hist'' = [ [show $ fst $ head histogramRow,
                                show $ length histogramRow]
                             | histogramRow <- hist' ]
                in showTable [rpad, rpad]
                             (["Number of reinstallations",
                               "Number of packages"] :
                              hist'')

dumpResults :: M ()
dumpResults
    = do st <- get
         liftIO $ writeFile (st_name st </> "buildable")
                            (unlines $ Set.toList $ st_buildablePackages st)
         liftIO $ writeFile (st_name st </> "buildFailed")
                            (unlines $ Set.toList $ st_buildFailurePackages st)
         liftIO $ writeFile (st_name st </> "buildDepsFailed")
                            (unlines $ Set.toList $ st_buildDepFailurePackages st)

--------------------------
-- Utils

die :: String -> M a
die err = liftIO
        $ do hPutStrLn stderr err
             exitWith (ExitFailure 1)

warn :: String -> M ()
warn msg = liftIO $ hPutStrLn stderr msg

runCmdGetStdout :: FilePath -> [String] -> M (Maybe String)
runCmdGetStdout prog args
    = liftIO
    $ do (hIn, hOut, hErr, ph) <- runInteractiveProcess prog args
                                                        Nothing Nothing
         hClose hIn
         mv <- newEmptyMVar
         sOut <- hGetContents hOut
         sErr <- hGetContents hErr
         _ <- forkIO $ (do _ <- evaluate (length sOut)
                           return ())
                        `finally`
                        putMVar mv ()
         _ <- forkIO $ (do _ <- evaluate (length sErr)
                           return ())
                        `finally`
                        putMVar mv ()
         ec <- waitForProcess ph
         takeMVar mv
         takeMVar mv
         case (ec, sErr) of
             (ExitSuccess, "") -> return $ Just sOut
             _ -> return Nothing

data StdLine = Stdout String
             | Stderr String

isStderr :: StdLine -> Bool
isStderr (Stdout _) = False
isStderr (Stderr _) = True

runCmdGetResults :: FilePath -> [String]
                 -> M (Either (ExitCode, [StdLine]) [String])
runCmdGetResults prog args
 = liftIO
 $ do (hIn, hOut, hErr, ph) <- runInteractiveProcess prog args Nothing Nothing
      hClose hIn
      linesMVar <- newEmptyMVar
      lineMVar <- newEmptyMVar
      let getLines h c = do l <- hGetLine h
                            putMVar lineMVar (Just (c l))
                            getLines h c
          writeLines :: Int -- how many of stdout and stderr are till open
                     -> [StdLine]
                     -> IO ()
          writeLines 0 ls = putMVar linesMVar (reverse ls)
          writeLines n ls = do mLine <- takeMVar lineMVar
                               case mLine of
                                   Just line -> writeLines n (line : ls)
                                   Nothing -> writeLines (n - 1) ls
      _ <- forkIO $ (do hSetBuffering hOut LineBuffering
                        getLines hOut Stdout `onEndOfFile` return ())
                     `finally`
                     putMVar lineMVar Nothing
      _ <- forkIO $ (do hSetBuffering hErr LineBuffering
                        getLines hErr Stderr `onEndOfFile` return ())
                     `finally`
                     putMVar lineMVar Nothing
      _ <- forkIO $ writeLines 2 []

      ec <- waitForProcess ph
      ls <- takeMVar linesMVar
      return $ case (ec, any isStderr ls) of
               (ExitSuccess, False) ->
                   Right [ sout | Stdout sout <- ls ]
               _ ->
                   Left (ec, ls)

