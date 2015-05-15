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
import System.Exit (ExitCode(..))

import BuildTools
import HackageMonad
import Utils

-- | Setup a package database.
initialisePackageConf :: FilePath -> Hkg ()
initialisePackageConf fp = do
    liftIO . ignoreException $ removeFile fp
    liftIO . ignoreException $ removeDirectoryRecursive fp
    x <- runGhcPkg ["init", fp]
    case x of
        ExitSuccess -> return ()
        _ -> die ("Initialising package database in " ++ show fp ++ " failed")

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

    rpath      <- getRunPath
    depFlags   <- getDepFlags
    pkgFlags   <- getPkgFlags
    basicFlags <- getBasicCabalFlags

    let cabalLog = rpath </> "logs.build" </> p <.> "cabal.log"
        deplog   = rpath </> "logs.build" </> p <.> "depends.log"
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
                    , "-j 4"
                      -- parallel build
                    ]
        depArgs = [ "install", p
                  , "--only-dependencies"
                  , "--build-log=" ++ deplog
                  ] ++ comFlags ++ depFlags

    -- try installing package dependencies
    xDeps <- runCabalResults False depArgs
    case xDeps of
        Right _ -> do
            let summaryName = rpath </> "logs.build" </> p <.> "summary"
                logName     = rpath </> "logs.build" </> p <.> "log"
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
    -- rmScratchDir p

  where
    toFile f strs = liftIO $ appendFile f (unlines strs)

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

    rpath      <- getRunPath
    basicFlags <- getBasicCabalFlags
    pkgFlags   <- getPkgFlags

    let summaryName = rpath </> "logs.stats" </> pkg <.> "summary"
        logName     = rpath </> "logs.stats" </> pkg <.> "log"
        resultName  = rpath </> "logs.stats" </> pkg <.> "result"
        args        = basicFlags ++ pkgFlags ++
                          [ "install", "--dry-run", "--reinstall", "--global"
                          , pkg , "--build-summary=" ++ summaryName
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
                die "===> Errror: You need to run 'cabal update' first!"

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
    notUpdated = any (isPrefixOf "Stderr: Run 'cabal update'")

    listHeaderPrefix = "In order, the following would be installed"
    noPackagesPrefix = "No packages to be installed."
    mangleLine l = case simpleParse . takeWhile (/= ' ') $ l of
                       Nothing ->
                           die ("Unparseable line: " ++ show l)
                       Just (PackageIdentifier (PackageName pn) _) ->
                           return pn

-- | Get the default cabal flags to use.
getBasicCabalFlags :: Hkg [String]
getBasicCabalFlags = do
    ghc    <- getGhc
    ghcPkg <- getGhcPkg
    return [ "--remote-build-reporting=none"
           , "--ghc"
           , "--with-ghc=" ++ ghc
           , "--with-ghc-pkg=" ++ ghcPkg
           ]

