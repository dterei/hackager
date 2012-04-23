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
import RecordOptions

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
        [        ] -> liftIO $ recordHelp (ExitFailure 1)
        ["--help"] -> liftIO $ recordHelp ExitSuccess
        _ -> do
            processArgs args
            ps <- getPkgs
            setupDir
            ps' <- case ps of
                    [] -> getPackages
                    _  -> return ps
            tryBuildingPackages ps'

