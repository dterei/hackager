-- | Hackage Test. Build all of hackage.
module Main (main) where

import Control.Monad.State
import System.Environment
import System.Exit
import System.IO

import BuildManager
import BuildTools
import HackageMonad

-- | Hackage Test (IO)
main :: IO ()
main = do
    st <- startState
    evalStateT main' st

-- | Hackage Test (Monad)
main' :: Hkg ()
main' = do
    liftIO $ hSetBuffering stdout NoBuffering
    args <- liftIO getArgs
    case args of
       name : cabalInstall : ghc : ghcPkg : depFlags : pkgFlags : ps -> do
           setupDir name
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

       ["help"  ] -> liftIO $ usageInfo ExitSuccess
       ["--help"] -> liftIO $ usageInfo ExitSuccess
       _          -> liftIO $ usageInfo (ExitFailure 1)

-- | Print usage information and exit
usageInfo :: ExitCode -> IO ()
usageInfo exitCode = do
    p <- getProgName
    mapM_ putStrLn [
        "Usage: " ++ p ++ " name cabalInstall ghc ghcPkg depFlags pkgFlags [pkgs]",
        "    name:         A name by which the results of this hackage test run will",
        "                  by referred, e.g. \"ghc-6.12.1\".",
        "    cabalInstall: The path to the cabal-install program to use.",
        "    ghc:          The path to the ghc program to use.",
        "    ghcPkg:       The path to the ghc-pkg program to use.",
        "    depFlags:     The flags to use when compiling dependencies of a package",
        "                  we are interested in, e.g. \"\" or \"-XFoo -XBar\".",
        "    pkgFlags:     The flags to use when compiling a package we are interested",
        "                  in, e.g. \"\" or \"-XFoo -XBar\".",
        "    pkgs:         An optional list of packages to build. If not specified, all",
        "                  of hackage is built. "
        ]
    exitWith exitCode

