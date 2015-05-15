-- | Tools for running hackager on many cores.
module Parallel (
        Child, forkChild, waitForChildren,
    ) where

import Control.Concurrent
import Control.Monad.State
import Control.Exception

import HackageMonad

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
