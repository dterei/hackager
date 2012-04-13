-- | Monad for Hackage Test. Just a simple state passing monad with appropriate
-- getter and setters.
module HackageMonad (
        PkgName, Hkg, HkgState, startState,

        setName, getName, getTempPackageConf,
        getScratchDir, getCabalInstall, setCabalInstall,
        getGhc, setGhc, getGhcPkg, setGhcPkg, getDepFlags, setDepFlags,
        getPkgFlags, setPkgFlags,

        addInstall, addInstalledPackage, addInstallablePackage,
        addNotInstallablePackage, addFailPackage,
        getInstallablePackages,
        buildSucceeded, buildFailed, buildDepsFailed,

        dumpStats, dumpResults
    ) where

import Control.Concurrent (MVar, newMVar)
import qualified Control.Concurrent as C
import Control.Monad.State
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.FilePath

import Utils

type PkgName = String

type Hkg = StateT HkgState IO

-- | The state of Hackager
data HkgState = HkgState {
        -- These are set based on the command line flags
        st_name         :: FilePath,
        st_dir          :: FilePath,
        st_cabalInstall :: FilePath,
        st_ghc          :: FilePath,
        st_ghcPkg       :: FilePath,
        st_depFlags     :: [String],
        st_pkgFlags     :: [String],

        -- These are set by the stats-collection pass:
        st_installedPackages      :: MVar (Set PkgName),
        st_installablePackages    :: MVar (Set PkgName),
        st_notInstallablePackages :: MVar (Set PkgName),
        st_failPackages           :: MVar (Set PkgName),
        st_installCounts          :: MVar (Map PkgName Int),

        -- These are set by the installation pass:
        st_buildablePackages       :: MVar (Set PkgName),
        st_buildFailurePackages    :: MVar (Set PkgName),
        st_buildDepFailurePackages :: MVar (Set PkgName)
    }

startState :: IO HkgState
startState = do
    ipkgs <- newMVar Set.empty
    apkgs <- newMVar Set.empty
    npkgs <- newMVar Set.empty
    fpkgs <- newMVar Set.empty
    count <- newMVar Map.empty
    bbpkgs <- newMVar Set.empty
    bfpkgs <- newMVar Set.empty
    bdpkgs <- newMVar Set.empty
    return $ HkgState {
        st_name                    = "",
        st_dir                     = "",
        st_cabalInstall            = "",
        st_ghc                     = "",
        st_ghcPkg                  = "",
        st_depFlags                = [],
        st_pkgFlags                = [],
        st_installedPackages       = ipkgs,
        st_installablePackages     = apkgs,
        st_notInstallablePackages  = npkgs,
        st_failPackages            = fpkgs,
        st_installCounts           = count,
        st_buildablePackages       = bbpkgs,
        st_buildFailurePackages    = bfpkgs,
        st_buildDepFailurePackages = bdpkgs
    }

------------------------------------------------
-- Helpers

setName :: FilePath -> Hkg ()
setName name = do
    st <- get
    dir <- liftIO getCurrentDirectory
    put $ st { st_name = name, st_dir = dir </> name }

getName :: Hkg FilePath
getName = get >>= \st -> return $ st_name st

getDir :: Hkg FilePath
getDir = get >>= \st -> return $ st_dir st

getTempPackageConf :: PkgName -> Hkg FilePath
getTempPackageConf p = getDir >>= \dir -> return $ dir </> p </> "temp.package.conf"

getScratchDir :: PkgName -> Hkg FilePath
getScratchDir p = getDir >>= \dir -> return $ dir </> "scratch" </> p

setCabalInstall :: FilePath -> Hkg ()
setCabalInstall ci = get >>= \st -> put $ st { st_cabalInstall = ci }

getCabalInstall :: Hkg FilePath
getCabalInstall = get >>= \st -> return $ st_cabalInstall st

setGhc :: FilePath -> Hkg ()
setGhc ghc = get >>= \st -> put $ st { st_ghc = ghc }

getGhc :: Hkg FilePath
getGhc = get >>= \st -> return $ st_ghc st

setGhcPkg :: FilePath -> Hkg ()
setGhcPkg ghcPkg = get >>= \st -> put $ st { st_ghcPkg = ghcPkg }

getGhcPkg :: Hkg FilePath
getGhcPkg = get >>= \st -> return $ st_ghcPkg st

setDepFlags :: String -> Hkg ()
setDepFlags depFlags = get >>= \st -> put $ st { st_depFlags = parseFlags depFlags }

getDepFlags :: Hkg [String]
getDepFlags = get >>= \st -> return $ st_depFlags st

setPkgFlags :: String -> Hkg ()
setPkgFlags pkgFlags = get >>= \st -> put $ st { st_pkgFlags = parseFlags pkgFlags }

getPkgFlags :: Hkg [String]
getPkgFlags = get >>= \st -> return $ st_pkgFlags st

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
    ipkgs <- takeMVar $ st_installedPackages st
    apkgs <- takeMVar $ st_installablePackages st
    npkgs <- takeMVar $ st_notInstallablePackages st
    fpkgs <- takeMVar $ st_failPackages st
    count <- takeMVar $ st_installCounts st

    let fullHistogram = reverse $ sort $ map swap
                      $ Map.assocs count
        (manyHistogram, fewHistogram) = span ((>= 10) . fst) fullHistogram
        total = sum $ map fst fullHistogram
        summaryTable = [ ["Num packages"           , show $ n]              
                       , ["Installed packages"     , show $ Set.size ipkgs]
                       , ["Installable packages"   , show $ Set.size apkgs]
                       , ["Uninstallable packages" , show $ Set.size npkgs]
                       , ["Failed packages"        , show $ Set.size fpkgs]
                       , ["Total reinstallations"  , show total]
                       ]

    name <- getName
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
    bpkgs <- takeMVar $ st_buildablePackages st
    fpkgs <- takeMVar $ st_buildFailurePackages st
    dpkgs <- takeMVar $ st_buildDepFailurePackages st

    liftIO $ writeFile (st_name st </> "buildable")
                       (unlines $ Set.toList bpkgs)
    liftIO $ writeFile (st_name st </> "buildFailed")
                       (unlines $ Set.toList fpkgs)
    liftIO $ writeFile (st_name st </> "buildDepsFailed")
                       (unlines $ Set.toList dpkgs)

takeMVar :: MVar a -> Hkg a
takeMVar m = liftIO $ C.takeMVar m

putMVar :: MVar a -> a -> Hkg ()
putMVar m v = liftIO $ C.putMVar m v

