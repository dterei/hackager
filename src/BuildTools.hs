-- | Wrappers to run various build tools needed.
module BuildTools (
        runCabal, runCabalResults, runGhcPkg
    ) where

import Control.Concurrent
import Control.Monad.State
import Control.Exception
import System.Exit (ExitCode(..))
import System.IO
import System.Process

import HackageMonad
import Utils

-- | Run cabal.
runCabal :: [String] -> Hkg ExitCode
runCabal args = do
    cabal <- getCabal
    liftIO $ rawSystem cabal args

-- | Run cabal returning the resulting output or error code
-- * Bool: Treat any stderr output as evidence that cabal failed
-- * [String]: The cabal arguments
runCabalResults :: Bool -> [String] -> Hkg (Either (ExitCode, [String]) [String])
runCabalResults errFail args = do
    cabal <- getCabal
    runCmdGetResults errFail cabal args

-- | Run ghc-pkg.
runGhcPkg :: [String] -> Hkg ExitCode
runGhcPkg args = do
    ghcPkg <- getGhcPkg
    liftIO $ rawSystem ghcPkg args

data StdLine = Stdout String | Stderr String

-- | Run a cmd returning the fullresults.
-- * Bool: Treat any stderr output as evidence the cmd failed
-- * FilePath: The program to run
-- * [String]: The program arguments
runCmdGetResults :: Bool -> FilePath -> [String]
                 -> Hkg (Either (ExitCode, [String]) [String])
runCmdGetResults errFail prog args = liftIO $ do
    (hIn, hOut, hErr, ph) <- runInteractiveProcess prog args Nothing Nothing
    hClose hIn
    linesMVar <- newEmptyMVar
    lineMVar <- newEmptyMVar
    let getLines h c = do l <- hGetLine h
                          putMVar lineMVar (Just (c l))
                          getLines h c

        writeLines :: Int -- how many of stdout and stderr are till open
                   -> [StdLine] -> IO ()
        writeLines 0 ls = putMVar linesMVar (reverse ls)
        writeLines n ls = do mLine <- takeMVar lineMVar
                             case mLine of
                                 Just line -> writeLines n (line : ls)
                                 Nothing   -> writeLines (n - 1) ls

    _ <- forkIO $ (hSetBuffering hOut LineBuffering >>
                   getLines hOut Stdout `onEndOfFile` return ())
                   `finally`
                   putMVar lineMVar Nothing

    _ <- forkIO $ (hSetBuffering hErr LineBuffering >>
                   getLines hErr Stderr `onEndOfFile` return ())
                   `finally`
                   putMVar lineMVar Nothing

    _ <- forkIO $ writeLines 2 []

    ec <- waitForProcess ph
    ls <- takeMVar linesMVar
    return $ case (ec, errFail && any isStderr ls) of
                (ExitSuccess, False) -> Right $ map stripResultLine ls
                _                    -> Left (ec, map mkResultLine ls)

  where
    isStderr (Stdout _) = False
    isStderr (Stderr _) = True

    stripResultLine (Stdout l) = l
    stripResultLine (Stderr l) = l

    mkResultLine (Stdout l) = "Stdout: " ++ l
    mkResultLine (Stderr l) = "Stderr: " ++ l

