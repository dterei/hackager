-- | Operations on a single package.
module Build (
        PkgProcessor,
        buildPkg,
        statPkg
    ) where

import Control.Monad.State
import Data.List
import Distribution.Package
import Distribution.Text
import System.Directory
import System.FilePath

import BuildTools
import HackageMonad
import Utils

-- | Type of a function that processes packages (and can be parallelized)
type PkgProcessor = Int -> Int -> PkgName -> Hkg ()

-- | Build a single package.
buildPkg :: PkgProcessor
buildPkg npkgs i p = do
    info $ "===> Building package " ++ p ++ " (" ++ show i ++ " of "
            ++ show npkgs ++ ")"

    tmpPackageConf <- getTempPackageConf p
    initialisePackageConf tmpPackageConf

    scratchDir <- getScratchDir p
    liftIO . ignoreException $ removeDirectoryRecursive scratchDir

    name         <- getName
    depFlags     <- getDepFlags
    pkgFlags     <- getPkgFlags
    basicFlags   <- getBasicCabalFlags

    let cabalLog = name </> "logs.build" </> p <.> "cabal.log"
        deplog   = name </> "logs.build" </> p <.> "depends.log"
        comFlags = basicFlags ++
                    [ "--prefix=" ++ scratchDir
                      -- This is the package database that we
                      -- want cabal to register into:
                    , "--package-db=" ++ tmpPackageConf
                      -- But cabal can't be trusted to put its
                      -- --package-conf flag after ours, so we need
                      -- to repeat the package.conf we want to
                      -- register into again:
                    , "--ghc-pkg-option=--package-conf=" ++ tmpPackageConf
                      -- Only we want to display info to the user
                    ]
        depArgs = [ "install", p
                  , "--only-dependencies"
                  , "--build-log=" ++ deplog
                  ] ++ comFlags ++ depFlags

    -- try installing package dependencies
    xDeps <- runCabalResults False depArgs
    case xDeps of
        Right _ -> do
            let summaryName = name </> "logs.build" </> p <.> "summary"
                logName     = name </> "logs.build" </> p <.> "log"
                pkgArgs     = [ "install", p
                              , "--build-summary=" ++ summaryName
                              , "--build-log=" ++ logName
                              ]
                              ++ comFlags ++ pkgFlags

            -- try installing package
            xPkg <- runCabalResults False pkgArgs
            case xPkg of
                Right _       -> buildSucceeded p
                Left (_, out) -> toFile cabalLog out >>
                                 buildFailed p

        Left (_, out) -> toFile cabalLog out >>
                         buildDepsFailed p

    -- clean up
    rmScratchDir p

  where
    toFile f strs = liftIO $ appendFile f (concat strs)

-- | Find out what would happen if we installed the package. Running
-- cabal-install in dry-run mode may tell us:
-- * The package is uninstallable
-- * The package is already installed
-- * The package is installable, and which other packages we would have to
--   install in order to install it
-- * Something we don't understand
statPkg :: PkgProcessor
statPkg npkgs i pkg = do
    info $ "===> Getting stats for: " ++ pkg ++ " (" ++ show i ++ " of "
            ++ show npkgs ++ ")"
    name <- getName
    basicFlags <- getBasicCabalFlags
    fs   <- getPkgFlags
    let summaryName = name </> "logs.stats" </> pkg <.> "summary"
        logName     = name </> "logs.stats" </> pkg <.> "log"
        resultName  = name </> "logs.stats" </> pkg <.> "result"
        args        = basicFlags ++ fs ++
                          [ "install", "--dry-run", pkg
                          , "--build-summary=" ++ summaryName
                          , "--build-log=" ++ logName
                          ]

    -- Ideally cabal would have written out some
    -- sort of log, but it seems not to do so in dry-run
    -- mode, so we do so ourselves
    e <- runCabalResults True args
    case e of
        Left (ec, ls) -> do
            let resultsLines = "FAILED"
                             : ("Exit code: " ++ show ec)
                             : ls
            liftIO $ writeFile resultName $ unlines resultsLines
            addNotInstallablePackage pkg
            info $ "===> " ++ pkg ++ " is not installable, skipping!"
            when (notUpdated resultsLines) $
                die ("===> Errror: You need to run 'cabal update' first!")

        Right ls -> do
            let resultsLines = "SUCCEEDED" : ls
            liftIO $ writeFile resultName $ unlines resultsLines
            case break (listHeaderPrefix `isPrefixOf`) ls of
                (_, _ : ls1) -> do
                    ls2 <- mapM mangleLine ls1
                    case reverse ls2 of
                        thisP : otherPs | thisP == pkg -> do
                            addInstallablePackage pkg
                            mapM_ addInstall otherPs

                        _ -> die ("Installing " ++ show pkg ++
                                  " doesn't end by installing it")

                _ | any (noPackagesPrefix `isPrefixOf`) ls
                  -> addInstalledPackage pkg

                  | otherwise
                  -> do warn $ "Don't understand result for " ++ show pkg
                        addFailPackage pkg

  where
    notUpdated []       = False
    notUpdated (x : xs) = if "Stderr: Run 'cabal update'" `isPrefixOf` x
                            then True
                            else notUpdated xs

    listHeaderPrefix = "In order, the following would be installed"
    noPackagesPrefix = "No packages to be installed."
    mangleLine l = case simpleParse l of
                       Nothing ->
                           die ("Unparseable line: " ++ show l)
                       Just (PackageIdentifier (PackageName pn) _) ->
                           return pn

-- | Get the default cabal flags to use.
getBasicCabalFlags :: Hkg [String]
getBasicCabalFlags = do
    ghc <- getGhc
    ghcPkg <- getGhcPkg
    return [ "--remote-build-reporting=none"
           , "--ghc"
           , "--with-ghc=" ++ ghc
           , "--with-ghc-pkg=" ++ ghcPkg
           ]

