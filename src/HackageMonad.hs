{-# LANGUAGE CPP #-}
-- | Monad for Hackage Test. Just a simple state passing monad with appropriate
-- getter and setters.
module HackageMonad (
        -- state holder types
        PkgName, Hkg, HkgState, startState,

        -- configure build paths and tools
        setRunPath, getRunPath, getCabal, setCabal, getGhc, setGhc, getGhcPkg,
        setGhcPkg, getCabalFlags, setCabalFlags, getDepFlags, setDepFlags,
        getRegex, setRegex, getPkgFlags, setPkgFlags,

        -- setup and retrieve list of packages to test
        addPkg, getPkgs,

        -- build scratch / isolated environment
        getTempPackageConf, getScratchDir, rmScratchDir, rmAllScratch,

        -- control build parallelism
        setThreads, getThreads,

        -- stats (what packages do we think we can and can't try to build?)
        addInstallablePackage, addNotInstallablePackage, addErrorPackage,
        addRevDepCounts,
        
        -- list of packages we've determined we can attempt to build
        getInstallablePackages,

        -- build outcomes
        buildSucceeded, buildFailed, buildDepsFailed,

        -- save results to disk
        dumpStats, dumpResults,

        -- logging utils
        info, warn, die
    ) where

import Control.Concurrent (MVar, newMVar)
import qualified Control.Concurrent as C
import Control.Monad.State
import Data.Function
#if __GLASGOW_HASKELL__ < 710
import Data.Functor
#endif
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import System.Exit (exitWith, ExitCode(..))
import System.IO

import Utils

type PkgName = String

type Hkg = StateT HkgState IO

-- | The state of Hackager
data HkgState = HkgState {
        -- These are set based on the command line flags
        st_dir      :: FilePath,
        st_cabal    :: FilePath,
        st_ghc      :: FilePath,
        st_ghcPkg   :: FilePath,
        st_cabFlags :: [String],
        st_depFlags :: [String],
        st_pkgFlags :: [String],
        st_threads  :: Int,
        st_regex    :: String,
        st_pkgs     :: Set PkgName,

        -- These are set by the stats-collection pass:
        st_installablePackages    :: MVar (Set PkgName),
        st_notInstallablePackages :: MVar (Set PkgName),
        st_errorPackages          :: MVar (Set PkgName),
        st_installCounts          :: MVar (Map PkgName Int),

        -- These are set by the installation pass:
        st_buildablePackages       :: MVar (Set PkgName),
        st_buildFailurePackages    :: MVar (Set PkgName),
        st_buildDepFailurePackages :: MVar (Set PkgName),

        -- internal locks for making stdout thread safe
        st_iolock :: MVar ()
    }

startState :: IO HkgState
startState = do
    apkgs  <- newMVar Set.empty
    npkgs  <- newMVar Set.empty
    fpkgs  <- newMVar Set.empty
    count  <- newMVar Map.empty
    bbpkgs <- newMVar Set.empty
    bfpkgs <- newMVar Set.empty
    bdpkgs <- newMVar Set.empty
    iolock <- newMVar ()
    return HkgState {
        st_dir                     = "",
        st_cabal                   = "",
        st_ghc                     = "",
        st_ghcPkg                  = "",
        st_cabFlags                = [],
        st_depFlags                = [],
        st_pkgFlags                = [],
        st_threads                 = 1,
        st_regex                   = "",
        st_pkgs                    = Set.empty,
        st_installablePackages     = apkgs,
        st_notInstallablePackages  = npkgs,
        st_errorPackages           = fpkgs,
        st_installCounts           = count,
        st_buildablePackages       = bbpkgs,
        st_buildFailurePackages    = bfpkgs,
        st_buildDepFailurePackages = bdpkgs,
        st_iolock                  = iolock
    }

------------------------------------------------
-- Helpers

setRunPath :: FilePath -> Hkg ()
setRunPath name = do
    dir <- liftIO getCurrentDirectory
    modify $ \st -> st { st_dir = dir </> name }

getRunPath :: Hkg FilePath
getRunPath = gets st_dir

getTempPackageConf :: PkgName -> Hkg FilePath
getTempPackageConf p = (<.> "package.conf") <$> getScratchDir p

getScratchDir :: PkgName -> Hkg FilePath
getScratchDir p = (</> "scratch" </> p) <$> getRunPath

rmScratchDir :: PkgName -> Hkg ()
rmScratchDir p = do
    dir <- getRunPath
    liftIO . ignoreException $
        removeDirectoryRecursive (dir </> "scratch" </> p)

rmAllScratch :: Hkg ()
rmAllScratch = do
    dir <- getRunPath
    liftIO . ignoreException $ removeDirectoryRecursive (dir </> "scratch")

getCabal :: Hkg FilePath
getCabal = gets st_cabal

setCabal :: FilePath -> Hkg ()
setCabal ci = modify $ \st -> st { st_cabal = ci }

getGhc :: Hkg FilePath
getGhc = gets st_ghc

setGhc :: FilePath -> Hkg ()
setGhc ghc = modify $ \st -> st { st_ghc = ghc }

getGhcPkg :: Hkg FilePath
getGhcPkg = gets st_ghcPkg

setGhcPkg :: FilePath -> Hkg ()
setGhcPkg ghcPkg = modify $ \st -> st { st_ghcPkg = ghcPkg }

getCabalFlags :: Hkg [String]
getCabalFlags = gets st_cabFlags

setCabalFlags :: String -> Hkg ()
setCabalFlags cf = modify $ \st -> st { st_cabFlags = parseFlags cf }

getDepFlags :: Hkg [String]
getDepFlags = gets st_depFlags

setDepFlags :: String -> Hkg ()
setDepFlags depFlags = modify $ \st -> st { st_depFlags = parseFlags depFlags }

getPkgFlags :: Hkg [String]
getPkgFlags = gets st_pkgFlags

setPkgFlags :: String -> Hkg ()
setPkgFlags pkgFlags = modify $ \st -> st { st_pkgFlags = parseFlags pkgFlags }

getPkgs :: Hkg [String]
getPkgs = gets $ Set.toList . st_pkgs

addPkg :: String -> Hkg ()
addPkg p = modify $ \st -> st { st_pkgs = Set.insert p (st_pkgs st) }

getThreads :: Hkg Int
getThreads = gets st_threads

setThreads :: Int -> Hkg ()
setThreads n = modify $ \st -> st { st_threads = n }

getRegex :: Hkg String
getRegex = gets st_regex

setRegex :: String -> Hkg ()
setRegex s = modify $ \st -> st { st_regex = s }


parseFlags :: String -> [String]
parseFlags str =
    case reads str of
        [(flags, "")] -> flags
        _             -> words str

addRevDepCounts :: PkgName -> Hkg ()
addRevDepCounts pn = do
    st <- get
    ics <- takeMVar $ st_installCounts st
    let ics' = Map.insertWith (+) pn 1 ics
    putMVar (st_installCounts st) ics'

addInstallablePackage :: PkgName -> Hkg ()
addInstallablePackage pkg = do
    st <- get
    s <- takeMVar $ st_installablePackages st
    putMVar (st_installablePackages st) $ Set.insert pkg s

getInstallablePackages :: Hkg [PkgName]
getInstallablePackages = do
    st <- get
    s <- takeMVar $ st_installablePackages st
    return $ Set.toList s

addNotInstallablePackage :: PkgName -> Hkg ()
addNotInstallablePackage pkg = do
    st <- get
    s <- takeMVar $ st_notInstallablePackages st
    putMVar (st_notInstallablePackages st) $ Set.insert pkg s 

addErrorPackage :: PkgName -> Hkg ()
addErrorPackage pkg = do
    st <- get
    s <- takeMVar $ st_errorPackages st
    putMVar (st_errorPackages st) $ Set.insert pkg s

buildSucceeded :: PkgName -> Hkg ()
buildSucceeded pkg = do
    st <- get
    s <- takeMVar $ st_buildablePackages st
    putMVar (st_buildablePackages st) $ Set.insert pkg s

buildFailed :: PkgName -> Hkg ()
buildFailed pkg = do
    st <- get
    s <- takeMVar $ st_buildFailurePackages st
    putMVar (st_buildFailurePackages st) $ Set.insert pkg s

buildDepsFailed :: PkgName -> Hkg ()
buildDepsFailed pkg = do
    st <- get
    s <- takeMVar $ st_buildDepFailurePackages st
    putMVar (st_buildDepFailurePackages st) $ Set.insert pkg s

dumpStats :: Int -> Hkg ()
dumpStats n = do
    st <- get
    apkgs <- readMVar $ st_installablePackages st
    npkgs <- readMVar $ st_notInstallablePackages st
    fpkgs <- readMVar $ st_errorPackages st
    count <- readMVar $ st_installCounts st

    let fullHistogram = sortBy (flip compare) (map swap $ Map.assocs count)
        total = Set.size apkgs + sum (map fst fullHistogram)
        summaryTable = [ ["Num packages" , show n]              
                       , ["Installable"  , show $ Set.size apkgs]
                       , ["Uninstallable", show $ Set.size npkgs]
                       , ["Errored"      , show $ Set.size fpkgs]
                       , ["Installations", show total]
                       ]

    rpath <- getRunPath
    liftIO $ do
        writeFile (rpath </> "stats.summary")
                  (unlines $ showTable [rpad, rpad] summaryTable)
        writeFile (rpath </> "stats.dependency-count")
                  (unlines $ showCompleteHistogram fullHistogram)
        writeFile (rpath </> "stats.dependency-histogram")
                  (unlines $ showSummaryHistogram fullHistogram)
        writeFile (rpath </> "packages.installable")
                  (unlines $ Set.toList apkgs)
        writeFile (rpath </> "packages.uninstallable")
                  (unlines $ Set.toList npkgs)
        writeFile (rpath </> "packages.error")
                  (unlines $ Set.toList fpkgs)

  where
    showCompleteHistogram hist = showTable [rpad, rpad]
                                           [ [show count, pkg]
                                           | (count, pkg) <- hist ]
    showSummaryHistogram hist =
        let hist' = groupBy (on (==) fst) hist
            hist'' = [ [show $ fst $ head histogramRow,
                        show $ length histogramRow]
                     | histogramRow <- hist' ]
        in showTable [rpad, rpad]
                     (["Reverse Dependencies",
                       "Number of Packages"] :
                      hist'')

dumpResults :: Hkg ()
dumpResults = do
    st <- get
    bpkgs <- readMVar $ st_buildablePackages st
    fpkgs <- readMVar $ st_buildFailurePackages st
    dpkgs <- readMVar $ st_buildDepFailurePackages st
    rpath <- getRunPath
    
    let total = Set.size bpkgs + Set.size fpkgs + Set.size dpkgs
        summaryTable = [ ["Attempted"  , show total]              
                       , ["Succeeded"  , show $ Set.size bpkgs]
                       , ["Failed"     , show $ Set.size fpkgs]
                       , ["Deps Failed", show $ Set.size dpkgs]
                       ]

    liftIO $ do
      writeFile (rpath </> "build.summary")
                (unlines $ showTable [rpad, rpad] summaryTable)
      writeFile (rpath </> "build.success") (unlines $ Set.toList bpkgs)
      writeFile (rpath </> "build.fail")    (unlines $ Set.toList fpkgs)
      writeFile (rpath </> "build.depfail") (unlines $ Set.toList dpkgs)

takeMVar :: MVar a -> Hkg a
takeMVar m = liftIO $ C.takeMVar m

putMVar :: MVar a -> a -> Hkg ()
putMVar m v = liftIO $ C.putMVar m v

readMVar :: MVar a -> Hkg a
readMVar m = liftIO $ C.readMVar m

-- | Print message to stdout.
info :: String -> Hkg ()
info msg = do
    l <- st_iolock <$> get
    void $ takeMVar l
    liftIO (putStrLn msg)
    putMVar l ()

-- | Print message to stderr.
warn :: String -> Hkg ()
warn msg = do
    l <- st_iolock <$> get
    void $ takeMVar l
    liftIO $ hPutStrLn stderr msg
    putMVar l ()

-- | Exit with error message.
die :: String -> Hkg a
die err = do
    l <- st_iolock <$> get
    void $ takeMVar l
    liftIO $ hPutStrLn stderr err
    putMVar l ()
    liftIO $ exitWith (ExitFailure 1)

