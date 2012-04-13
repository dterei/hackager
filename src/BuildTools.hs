-- | Various tools and utility functions to do wtih building.
module BuildTools (
        Child, forkChild, waitForChildren,
        setupDir,
        runCabal, runCabalResults, runCabalStdout,
        runGhcPkg,
        initialisePackageConf,
        die, info, warn,
        StdLine(..)
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

-- | Run cabal.
runCabal :: [String] -> Hkg ExitCode
runCabal args = do
    cabalInstall <- getCabalInstall
    x <- liftIO $ rawSystem cabalInstall args
    return x

-- | Run cabal returing the results
runCabalStdout :: [String] -> Hkg (Maybe String)
runCabalStdout args = do
    cabalInstall <- getCabalInstall
    r <- runCmdGetStdout cabalInstall args
    return r

-- | Run cabal returning the resulting output or error code
runCabalResults :: [String] -> Hkg (Either (ExitCode, [StdLine]) [String])
runCabalResults args = do
    cabalInstall <- getCabalInstall
    r <- runCmdGetResults cabalInstall args
    return r

-- | Run ghc-pkg.
runGhcPkg :: [String] -> Hkg ExitCode
runGhcPkg args = do
    ghcPkg <- getGhcPkg
    x <- liftIO $ rawSystem ghcPkg args
    return x

-- | Setup a package database
initialisePackageConf :: FilePath -> Hkg ()
initialisePackageConf fp = do
    liftIO . ignoreException $ removeFile fp
    liftIO . ignoreException $ removeDirectoryRecursive fp
    x <- runGhcPkg ["init", fp]
    case x of
        ExitSuccess -> return ()
        _ -> die ("Initialising package database in " ++ show fp ++ " failed")

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

-- | Command output representation
data StdLine = Stdout String
             | Stderr String

-- | Run a cmd return its stdout results.
runCmdGetStdout :: FilePath -> [String] -> Hkg (Maybe String)
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

-- | Run a cmd returning the fullresults.
runCmdGetResults :: FilePath -> [String]
                 -> Hkg (Either (ExitCode, [StdLine]) [String])
runCmdGetResults prog args = liftIO $ do
    (hIn, hOut, hErr, ph) <- runInteractiveProcess prog args Nothing Nothing
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
    return $ case (ec, any isStderr ls) of
                (ExitSuccess, False) -> Right [ sout | Stdout sout <- ls ]
                _                    -> Left (ec, ls)

  where
    isStderr (Stdout _) = False
    isStderr (Stderr _) = True

