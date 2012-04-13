
module Build (buildInCommon, doPackageBuild,
              initialisePackageConf, getBasicCabalInstallFlags) where

import HackageTestMonad
import Utils

import Control.Monad.State
import System.Directory
import System.Exit
import System.FilePath
import System.Process

buildInCommon :: [PkgName] -> M ()
buildInCommon ps
    = do commonPackageConf <- getCommonPackageConf
         commonPrefix <- getCommonPrefix
         cabalInstall <- getCabalInstall
         basicFlags <- getBasicCabalInstallFlags
         depFlags <- getDepFlags
         let extraFlags = ["--prefix=" ++ commonPrefix,
                           "--package-db=" ++ commonPackageConf]
         let args = ["install"] ++ ps ++ basicFlags ++ extraFlags ++ depFlags
         ecDeps <- liftIO $ rawSystem cabalInstall args
         case ecDeps of
             ExitSuccess ->
                 return ()
             _ ->
                 die "Can't install common deps together"

doPackageBuild :: Int -> PkgName -> Int -> M ()
doPackageBuild num p n
    = do liftIO $ putStrLn ("===> " ++ p ++
                            " (" ++ show n ++ " of " ++ show num ++ ")")
         commonPackageConf <- getCommonPackageConf
         tempPackageConf <- getTempPackageConf
         scratchDir <- getScratchDir
         initialisePackageConf tempPackageConf
         liftIO (removeDirectoryRecursive scratchDir `catchIO` \_ -> return ())
         cabalInstall <- getCabalInstall
         name <- getName
         depFlags <- getDepFlags
         pkgFlags <- getPkgFlags
         basicFlags <- getBasicCabalInstallFlags
         let commonFlags = basicFlags ++
                           ["--prefix=" ++ scratchDir,
                            -- This is the package database that we
                            -- want cabal to register into:
                            "--package-db=" ++ tempPackageConf,
                            -- but we also need GHC to be able to use
                            -- packages from the common database:
                            "--ghc-option=-package-conf=" ++ commonPackageConf,
                            -- and likewise ghc-pkg needs to know what
                            -- packages are in there:
                            "--ghc-pkg-option=--package-conf=" ++
                             commonPackageConf,
                            -- but cabal can't be trusted to put its
                            -- --package-conf flag after ours, so we need
                            -- to repeat the package.conf we want to
                            -- register into again:
                            "--ghc-pkg-option=--package-conf=" ++
                             tempPackageConf]
         let depArgs = ["install", p, "--only-dependencies"]
                    ++ commonFlags ++ depFlags
         ecDeps <- liftIO $ rawSystem cabalInstall depArgs
         case ecDeps of
             ExitSuccess ->
                 do let summaryName = name </> "logs.build" </> p <.> "summary"
                        logName     = name </> "logs.build" </> p <.> "log"
                        pkgArgs = ["install", p,
                                   "--build-summary=" ++ summaryName,
                                   "--build-log=" ++ logName]
                               ++ commonFlags ++ pkgFlags
                    ecPkg <- liftIO $ rawSystem cabalInstall pkgArgs
                    case ecPkg of
                        ExitSuccess ->
                            buildSucceeded p
                        _ ->
                            buildFailed p
             _ ->
                 buildDepsFailed p

initialisePackageConf :: FilePath -> M ()
initialisePackageConf fp
 = do liftIO (removeFile               fp `catchIO` \_ -> return ())
      liftIO (removeDirectoryRecursive fp `catchIO` \_ -> return ())
      ghcPkg <- getGhcPkg
      ec <- liftIO $ rawSystem ghcPkg ["init", fp]
      case ec of
          ExitSuccess ->
              return ()
          _ ->
              die ("Initialising package database in " ++ show fp ++ " failed")

getBasicCabalInstallFlags :: M [String]
getBasicCabalInstallFlags
    = do ghc <- getGhc
         ghcPkg <- getGhcPkg
         return ["--remote-build-reporting=none",
                 "--ghc",
                 "--with-ghc=" ++ ghc,
                 "--with-ghc-pkg=" ++ ghcPkg]

