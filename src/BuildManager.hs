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
    info "===> Grabbing a list of all packages on hackage..."
    m <- runCabalResults False ["list", "--simple-output", "-v0"]
    -- m: abc 0.0.1
    --    abc 0.0.2
    --    dff 0.1.2
    --    ...
    case m of
        Left (_, out) -> die $ "Failed to get package list: " ++ concat out
        Right xs ->
            let ls = map (takeWhile (' ' /=)) $ xs
                ps = uniq $ filter (not . null) ls
            in return ps

-- | Loop over given packages and try to build each of them, recording the
-- results.
tryBuildingPackages :: Int -> [PkgName] -> Hkg ()
tryBuildingPackages nthreads ps = do
    let n = length ps
    info $ "===> Testing against " ++ show n ++ " packages..."
    runOnAllPkgs nthreads ps statPkg
    dumpStats n
    psAll    <- getInstallablePackages
    runOnAllPkgs nthreads psAll buildPkg
    dumpResults
    info $ "===> Hackager finished! (" ++ show n ++ " packages tested)"

-- | Run in parallel a PkgProcessor function over the given list
-- of packages with the specified number of threads.
runOnAllPkgs :: Int -> [PkgName] -> PkgProcessor -> Hkg ()
runOnAllPkgs nthreads pkgs pkgFun = do
    mpkgs <- liftIO . newMVar $ zip [1..] pkgs
    let runner = builder pkgFun (length pkgs) mpkgs
    children <- replicateM nthreads $ forkChild runner
    waitForChildren children
    info ""
    info ""

-- | Function to go through a package list and build them. Thread safe so can
-- be forked as child processes.
builder :: PkgProcessor -> Int -> MVar [(Int, PkgName)] -> Hkg ()
builder pkgFun n mpkgs = go
  where
    go = do
        pkgs <- liftIO $ takeMVar mpkgs
        case pkgs of
            []            -> liftIO (putMVar mpkgs []) >> return ()
            ((i, p) : ps) -> do
                liftIO $ putMVar mpkgs ps
                pkgFun n i p
                go

