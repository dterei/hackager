-- | Operations on a single package.
module Build (
        buildPkg,
        statPkg
    ) where

import BuildTools
import HackageMonad
import Utils

import Control.Monad.State
import Data.List
import Distribution.Package
import Distribution.Text
import System.Directory
import System.Exit
import System.FilePath

-- | Build a single package.
buildPkg :: Int -> PkgName -> Int -> Hkg ()
buildPkg npkgs p i = do
    info $ "===> " ++ p ++ " (" ++ show i ++ " of " ++ show npkgs ++ ")"

    tmpPackageConf <- getTempPackageConf
    initialisePackageConf tmpPackageConf

    scratchDir <- getScratchDir
    liftIO . ignoreException $ removeDirectoryRecursive scratchDir

    name         <- getName
    depFlags     <- getDepFlags
    pkgFlags     <- getPkgFlags
    basicFlags   <- getBasicCabalInstallFlags

    let comFlags = basicFlags ++
                    [ "--prefix=" ++ scratchDir
                      -- This is the package database that we
                      -- want cabal to register into:
                    , "--package-db=" ++ tmpPackageConf
                      -- but cabal can't be trusted to put its
                      -- --package-conf flag after ours, so we need
                      -- to repeat the package.conf we want to
                      -- register into again:
                    , "--ghc-pkg-option=--package-conf=" ++ tmpPackageConf
                    ]
        depArgs = ["install", p, "--only-dependencies"] ++ comFlags ++ depFlags

    -- try installing package dependencies
    xDeps <- runCabal depArgs
    case xDeps of
        ExitSuccess -> do
            let summaryName = name </> "logs.build" </> p <.> "summary"
                logName     = name </> "logs.build" </> p <.> "log"
                pkgArgs     = [ "install", p
                              , "--build-summary=" ++ summaryName
                              ,  "--build-log=" ++ logName
                              ]
                              ++ comFlags ++ pkgFlags

            -- try installing package
            xPkg <- runCabal pkgArgs
            case xPkg of
                ExitSuccess -> buildSucceeded p
                _           -> buildFailed p

        _ -> buildDepsFailed p

-- | Find out what would happen if we installed the package. Running
-- cabal-install in dry-run mode may tell us:
-- * The package is uninstallable
-- * The package is already installed
-- * The package is installable, and which other packages we would have to
--   install in order to install it
-- * Something we don't understand
statPkg :: Int -> PkgName -> Int -> Hkg ()
statPkg npkgs pkg i = do
    info $ "\r" ++ show i ++ " of " ++ show npkgs
    name <- getName
    basicFlags <- getBasicCabalInstallFlags
    fs   <- getPkgFlags
    let summaryName = name </> "logs.stats" </> pkg <.> "summary"
        logName     = name </> "logs.stats" </> pkg <.> "log"
        resultName  = name </> "logs.stats" </> pkg <.> "result"
        args        = basicFlags ++ fs ++
                          [ "install", "--dry-run", pkg
                          , "--build-summary=" ++ summaryName
                          , "--build-log=" ++ logName
                          ]

    -- Ideally cabal-install would have written out some
    -- sort of log, but it seems not to do so in dry-run
    -- mode, so we do so ourselves
    e <- runCabalResults args
    case e of
        Left (ec, ls) -> do
            let resultsLines = "FAILED"
                             : ("Exit code: " ++ show ec)
                             : map mkResultLine ls
                mkResultLine (Stdout l) = "Stdout: " ++ l
                mkResultLine (Stderr l) = "Stderr: " ++ l
            liftIO $ writeFile resultName $ unlines resultsLines
            addNotInstallablePackage pkg

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
    listHeaderPrefix = "In order, the following would be installed"
    noPackagesPrefix = "No packages to be installed."
    mangleLine l = case simpleParse l of
                       Nothing ->
                           die ("Unparseable line: " ++ show l)
                       Just (PackageIdentifier (PackageName pn) _) ->
                           return pn

-- | Get the default cabal flags to use.
getBasicCabalInstallFlags :: Hkg [String]
getBasicCabalInstallFlags = do
    ghc <- getGhc
    ghcPkg <- getGhcPkg
    return [ "--remote-build-reporting=none"
           , "--ghc"
           , "--with-ghc=" ++ ghc
           , "--with-ghc-pkg=" ++ ghcPkg
           ]

