-- | Handle building a set of packages (usually all of Hackage)
module BuildManager (
        setupBuildDir,
        getAllHackage,
        tryBuildingPackages
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import System.Directory
import System.FilePath

import Build
import BuildTools
import HackageMonad
import Parallel
import Utils

-- | Setup the needed directory structure.
setupBuildDir:: Hkg ()
setupBuildDir = do
    rpath <- getRunPath
    exists <- liftIO $ doesDirectoryExist rpath
    if exists
        then die (show rpath ++ " already exists, not overwriting")
        else liftIO $ do
            createDirectory rpath
            createDirectory (rpath </> "logs.stats")
            createDirectory (rpath </> "logs.build")

-- | Get a list of all packages on hackage.
getAllHackage :: Hkg [PkgName]
getAllHackage = do
    info "===> Grabbing a list of all packages on hackage..."
    m <- runCabalResults False ["list", "--simple-output", "-v0"]
    -- m: abc 0.0.1
    --    abc 0.0.2
    --    dff 0.1.2
    --    ...
    case m of
        Left (_, out) -> die $ "Failed to get package list: " ++ unlines out
        Right xs ->
            let ls = map (takeWhile (' ' /=)) xs
                ps = uniq $ filter (not . null) ls
            in return ps

-- | Loop over given packages and try to build each of them, recording the
-- results.
tryBuildingPackages :: [PkgName] -> Hkg ()
tryBuildingPackages ps = do
    let n = length ps
    info $ "===> Testing against " ++ show n ++ " packages..."
    runOnAllPkgs ps statPkg
    dumpStats n
    psAll <- getInstallablePackages
    runOnAllPkgs psAll buildPkg
    dumpResults
    -- rmAllScratch
    info $ "===> Hackager finished! (" ++ show n ++ " packages tested)"

-- | Run in parallel a PkgProcessor function over the given list
-- of packages with the specified number of threads.
runOnAllPkgs :: [PkgName] -> PkgProcessor -> Hkg ()
runOnAllPkgs pkgs pkgFun = do
    nthreads <- getThreads
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
            []            -> liftIO $ putMVar mpkgs []
            ((i, p) : ps) -> do
                liftIO $ putMVar mpkgs ps
                pkgFun n i p
                go

