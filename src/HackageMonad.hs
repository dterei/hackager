{-# LANGUAGE CPP #-}
-- | Monad for Hackage Test. Just a simple state passing monad with appropriate
-- getter and setters.
module HackageMonad (
        PkgName, Hkg, HkgState, startState,

        getTempPackageConf, getScratchDir, rmScratchDir, rmAllScratch,

        setRunPath, getRunPath, getCabal, setCabal, getGhc, setGhc, getGhcPkg,
        setGhcPkg, getCabalFlags, setCabalFlags, getDepFlags, setDepFlags,
        getPkgFlags, setPkgFlags, addPkg, getPkgs,

        setThreads, getThreads,

        addInstall, addInstalledPackage, addInstallablePackage,
        addNotInstallablePackage, addFailPackage, getInstallablePackages,
        buildSucceeded, buildFailed, buildDepsFailed,

        dumpStats, dumpResults,

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
        st_pkgs     :: Set PkgName,

        -- These are set by the stats-collection pass:
        st_installedPackages      :: MVar (Set PkgName),
        st_installablePackages    :: MVar (Set PkgName),
        st_notInstallablePackages :: MVar (Set PkgName),
        st_failPackages           :: MVar (Set PkgName),
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
    ipkgs  <- newMVar Set.empty
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
        st_pkgs                    = Set.empty,
        st_installedPackages       = ipkgs,
        st_installablePackages     = apkgs,
        st_notInstallablePackages  = npkgs,
        st_failPackages            = fpkgs,
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

setCabal :: FilePath -> Hkg ()
setCabal ci = modify $ \st -> st { st_cabal = ci }

getCabal :: Hkg FilePath
getCabal = gets st_cabal

setGhc :: FilePath -> Hkg ()
setGhc ghc = modify $ \st -> st { st_ghc = ghc }

getGhc :: Hkg FilePath
getGhc = gets st_ghc

setGhcPkg :: FilePath -> Hkg ()
setGhcPkg ghcPkg = modify $ \st -> st { st_ghcPkg = ghcPkg }

getGhcPkg :: Hkg FilePath
getGhcPkg = gets st_ghcPkg

setCabalFlags :: String -> Hkg ()
setCabalFlags cf = modify $ \st -> st { st_cabFlags = parseFlags cf }

getCabalFlags :: Hkg [String]
getCabalFlags = gets st_cabFlags

setDepFlags :: String -> Hkg ()
setDepFlags depFlags = modify $ \st -> st { st_depFlags = parseFlags depFlags }

getDepFlags :: Hkg [String]
getDepFlags = gets st_depFlags

setPkgFlags :: String -> Hkg ()
setPkgFlags pkgFlags = modify $ \st -> st { st_pkgFlags = parseFlags pkgFlags }

getPkgFlags :: Hkg [String]
getPkgFlags = gets st_pkgFlags

addPkg :: String -> Hkg ()
addPkg p = modify $ \st -> st { st_pkgs = Set.insert p (st_pkgs st) }

getPkgs :: Hkg [String]
getPkgs = gets $ Set.toList . st_pkgs

setThreads :: Int -> Hkg ()
setThreads n = modify $ \st -> st { st_threads = n }

getThreads :: Hkg Int
getThreads = gets st_threads

parseFlags :: String -> [String]
parseFlags str =
    case reads str of
        [(flags, "")] -> flags
        _             -> words str

addInstall :: PkgName -> Hkg ()
addInstall pn = do
    st <- get
    ics <- takeMVar $ st_installCounts st
    let ics' = Map.insertWith (+) pn 1 ics
    putMVar (st_installCounts st) ics'

addInstalledPackage :: PkgName -> Hkg ()
addInstalledPackage pkg = do
    st <- get
    s  <- takeMVar $ st_installedPackages st
    putMVar (st_installedPackages st) $ Set.insert pkg s

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

addFailPackage :: PkgName -> Hkg ()
addFailPackage pkg = do
    st <- get
    s <- takeMVar $ st_failPackages st
    putMVar (st_failPackages st) $ Set.insert pkg s

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
    ipkgs <- readMVar $ st_installedPackages st
    apkgs <- readMVar $ st_installablePackages st
    npkgs <- readMVar $ st_notInstallablePackages st
    fpkgs <- readMVar $ st_failPackages st
    count <- readMVar $ st_installCounts st

    let fullHistogram = sortBy (flip compare) (map swap $ Map.assocs count)
        (manyHistogram, fewHistogram) = span ((>= 10) . fst) fullHistogram
        total = sum $ map fst fullHistogram
        summaryTable = [ ["Num packages"           , show n]              
                       , ["Installed packages"     , show $ Set.size ipkgs]
                       , ["Installable packages"   , show $ Set.size apkgs]
                       , ["Uninstallable packages" , show $ Set.size npkgs]
                       , ["Failed packages"        , show $ Set.size fpkgs]
                       , ["Total reinstallations"  , show total]
                       ]

    name <- getRunPath
    liftIO $ do
        writeFile (name </> "stats.full")
                  (unlines $ showCompleteHistogram fullHistogram)
        writeFile (name </> "stats.many")
                  (unlines $ showCompleteHistogram manyHistogram)
        writeFile (name </> "stats.few")
                  (unlines $ showSummaryHistogram fewHistogram)
        writeFile (name </> "stats.summary")
                  (unlines $ showTable [rpad, rpad] summaryTable)
        writeFile (name </> "installed-packages")
                  (unlines $ Set.toList ipkgs)
        writeFile (name </> "installable-packages")
                  (unlines $ Set.toList apkgs)
        writeFile (name </> "uninstallable-packages")
                  (unlines $ Set.toList npkgs)
        writeFile (name </> "fail-packages")
                  (unlines $ Set.toList fpkgs)
        writeFile (name </> "install-counts")
                  (unlines $ map show $ Map.assocs count)

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
                     (["Number of reinstallations",
                       "Number of packages"] :
                      hist'')

dumpResults :: Hkg ()
dumpResults = do
    st <- get
    bpkgs <- readMVar $ st_buildablePackages st
    fpkgs <- readMVar $ st_buildFailurePackages st
    dpkgs <- readMVar $ st_buildDepFailurePackages st
    rpath <- getRunPath

    liftIO $ writeFile (rpath </> "buildable")
                       (unlines $ Set.toList bpkgs)
    liftIO $ writeFile (rpath </> "buildFailed")
                       (unlines $ Set.toList fpkgs)
    liftIO $ writeFile (rpath </> "buildDepsFailed")
                       (unlines $ Set.toList dpkgs)

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

