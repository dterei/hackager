-- | Handle building a set of packages (usually all of Hackage)
module BuildManager (
        getPackages,
        tryBuildingPackages
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.State

import Build
import BuildTools
import HackageMonad
import Utils

-- | Get a list of all packages on hackage.
getPackages :: Hkg [PkgName]
getPackages = do
    m <- runCabalStdout ["list", "--simple-output", "-v0"]
    -- m: abc 0.0.1
    --    abc 0.0.2
    --    dff 0.1.2
    --    ...
    case m of
        Nothing -> die "Failed to get package list"
        Just xs ->
            let ls = map (takeWhile (' ' /=)) $ lines xs
                ps = uniq $ filter (not . null) ls
            in return ps

-- | Loop over given packages and try to build each of them, recording the
-- results.
tryBuildingPackages :: Int -> [PkgName] -> Hkg ()
tryBuildingPackages nthreads ps = do
    -- Our main objective here is to find out how many times each package would
    -- be installed as a dependency of another package.
    zipWithM_ (statPkg $ length ps) ps [1..]
    info ""
    info ""
    -- Write out the data so we can look at it (by hand) later if we want.
    dumpStats (length ps)
    psAll    <- getInstallablePackages
    mpsAll   <- liftIO . newMVar $ zip [1..] psAll
    children <- replicateM nthreads (forkChild $ builder (length psAll) mpsAll)
    waitForChildren children
    dumpResults

-- | Function to go through a package list and build them. Thread safe so can
-- be forked as child processes.
builder :: Int -> MVar [(Int, PkgName)] -> Hkg ()
builder n mpkgs = go
  where
    go = do
        pkgs <- liftIO $ takeMVar mpkgs
        case pkgs of
            []            -> liftIO (putMVar mpkgs []) >> return ()
            ((i, p) : ps) -> do
                liftIO $ putMVar mpkgs ps
                buildPkg n i p
                go

