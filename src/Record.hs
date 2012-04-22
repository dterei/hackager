-- | Record tool. Build all of hackage.
module Record (
        record,
        recordHelp
    ) where

import Control.Monad.State
import System.Exit
import System.IO

import BuildManager
import BuildTools
import HackageMonad

-- | Run the recording tool.
record :: [String] -> IO ()
record args = do
    st <- startState
    evalStateT (mainST args) st

-- | Hackage Test (Monad).
mainST :: [String] -> Hkg ()
mainST args = do
    liftIO $ hSetBuffering stdout NoBuffering
    case args of
       name : cabalInstall : ghc : ghcPkg : depFlags : pkgFlags : threads : ps -> do
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
           tryBuildingPackages (read threads) ps'

       ["--help"] -> liftIO $ recordHelp ExitSuccess
       [        ] -> liftIO $ recordHelp (ExitFailure 1)
       _          -> liftIO $ recordHelp (ExitFailure 1)

-- | Print usage information and exit
recordHelp :: ExitCode -> IO ()
recordHelp exitCode = do
    mapM_ putStrLn
        [ "usage: hackager record <name> <cabal> <ghc> <ghc-pkg> <dep-flags>"
        , "                       <pkg-flags> <threads> [pkgs]"
        , ""
        , "    name         A name by which the results of this Hackager run will"
        , "                 be referred, e.g. \"ghc-6.12.1\""
        , "    cabal        The path to the cabal program to use"
        , "    ghc          The path to the ghc program to use"
        , "    ghc-pkg      The path to the ghc-pkg program to use"
        , "    dep-flags    The flags to use when compiling dependencies of a package"
        , "                 e.g. \"\" or \"-XFoo -XBar\""
        , "    pkg-flags    The flags to use when compiling a package"
        , "                 e.g. \"\" or \"-XFoo -XBar\""
        , "    threads      Number of threads to use to build in parallel"
        , "    pkgs         An optional list of packages to build. If not specified"
        , "                 all of hackage is built"
        ]
    exitWith exitCode

