-- | Various tools and utility functions to do wtih building.
module BuildTools (
        die, info, warn,
        Child, forkChild, waitForChildren,
        setupDir, initialisePackageConf,
        runCabal, runCabalResults, runGhcPkg
    ) where

import Control.Concurrent
import Control.Monad.State
import Control.Exception
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import HackageMonad
import Utils

-- | Print message to stdout.
info :: String -> Hkg ()
info msg = getIOLock >> liftIO (putStrLn msg) >> releaseIOLock

-- | Print message to stderr.
warn :: String -> Hkg ()
warn msg = getIOLock >> liftIO (hPutStrLn stderr msg) >> releaseIOLock

-- | Exit with error message.
die :: String -> Hkg a
die err = do
    getIOLock
    liftIO $ hPutStrLn stderr err
    releaseIOLock
    liftIO $ exitWith (ExitFailure 1)

-- | Children process signal
type Child = MVar ()

-- | Fork a child and return an MVar that singals when the child is done.
forkChild :: Hkg () -> Hkg Child
forkChild hkg = do
    mvar <- liftIO $ newEmptyMVar
    st <- get
    _ <- liftIO $ forkIO (evalStateT hkg st `finally` putMVar mvar ())
    return mvar

-- | Wait on a list of children to finish processing
waitForChildren :: [Child] -> Hkg ()
waitForChildren []                 = return ()
waitForChildren (child : children) = do
    liftIO $ takeMVar child
    waitForChildren children

-- | Setup the needed directory structure
setupDir:: String -> Hkg ()
setupDir name = do
    exists <- liftIO $ doesDirectoryExist name
    if exists
        then die (show name ++ " already exists, not overwriting")
        else liftIO $ do
            createDirectory name
            createDirectory (name </> "logs.stats")
            createDirectory (name </> "logs.build")

-- | Setup a package database
initialisePackageConf :: FilePath -> Hkg ()
initialisePackageConf fp = do
    liftIO . ignoreException $ removeFile fp
    liftIO . ignoreException $ removeDirectoryRecursive fp
    x <- runGhcPkg ["init", fp]
    case x of
        ExitSuccess -> return ()
        _ -> die ("Initialising package database in " ++ show fp ++ " failed")

-- | Run cabal.
runCabal :: [String] -> Hkg ExitCode
runCabal args = do
    cabalInstall <- getCabalInstall
    x <- liftIO $ rawSystem cabalInstall args
    return x

-- | Run cabal returning the resulting output or error code
-- * Bool: Treat any stderr output as evidence that cabal failed
-- * [String]: The cabal arguments
runCabalResults :: Bool -> [String] -> Hkg (Either (ExitCode, [String]) [String])
runCabalResults errFail args = do
    cabalInstall <- getCabalInstall
    r <- runCmdGetResults errFail cabalInstall args
    return r

-- | Run ghc-pkg.
runGhcPkg :: [String] -> Hkg ExitCode
runGhcPkg args = do
    ghcPkg <- getGhcPkg
    x <- liftIO $ rawSystem ghcPkg args
    return x

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
    return $ case (ec, errFail && (any isStderr ls)) of
                (ExitSuccess, False) -> Right $ map stripResultLine ls
                _                    -> Left (ec, map mkResultLine ls)

  where
    isStderr (Stdout _) = False
    isStderr (Stderr _) = True

    stripResultLine (Stdout l) = l
    stripResultLine (Stderr l) = l

    mkResultLine (Stdout l) = "Stdout: " ++ l
    mkResultLine (Stderr l) = "Stderr: " ++ l

