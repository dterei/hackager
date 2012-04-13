-- | Monad for Hackage Test. Just a simple state passing monad with appropriate
-- getter and setters.
module HackageMonad where

import Utils

import Control.Monad.State
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (catch)
import System.Directory
import System.FilePath

type PkgName = String

type Hkg = StateT HkgState IO

-- We're a bit sloppy with this type. Different fields only get good
-- values at different phases.
data HkgState = HkgState {
        -- These are set based on the command line flags
        st_name :: FilePath,
        st_dir :: FilePath,
        st_cabalInstall :: FilePath,
        st_ghc :: FilePath,
        st_ghcPkg :: FilePath,
        st_depFlags :: [String],
        st_pkgFlags :: [String],
        -- These are set by the stats-collection pass:
        st_installedPackages :: Set PkgName,
        st_installablePackages :: Set PkgName,
        st_notInstallablePackages :: Set PkgName,
        st_failPackages :: Set PkgName,
        st_installCounts :: Map PkgName Int,
        -- These are set by the installation pass:
        st_buildablePackages :: Set PkgName,
        st_buildFailurePackages :: Set PkgName,
        st_buildDepFailurePackages :: Set PkgName
    }

startState :: HkgState
startState = HkgState {
        st_name = "",
        st_dir = "",
        st_cabalInstall = "",
        st_ghc = "",
        st_ghcPkg = "",
        st_depFlags = [],
        st_pkgFlags = [],
        st_installedPackages = Set.empty,
        st_installablePackages = Set.empty,
        st_notInstallablePackages = Set.empty,
        st_failPackages = Set.empty,
        st_installCounts = Map.empty,
        st_buildablePackages = Set.empty,
        st_buildFailurePackages = Set.empty,
        st_buildDepFailurePackages = Set.empty
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

getCommonPackageConf :: Hkg FilePath
getCommonPackageConf = getDir >>= \dir -> return $ dir </> "common.package.conf"

getCommonPrefix :: Hkg FilePath
getCommonPrefix = getDir >>= \dir -> return $ dir </> "commonPrefix"

getTempPackageConf :: Hkg FilePath
getTempPackageConf = getDir >>= \dir -> return $ dir </> "temp.package.conf"

getScratchDir :: Hkg FilePath
getScratchDir = getDir >>= \dir -> return $ dir </> "scratch"

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
    let ics = Map.insertWith (+) pn 1 $ st_installCounts st
    put $ st { st_installCounts = ics }

addInstalledPackage :: PkgName -> Hkg ()
addInstalledPackage pkg = do
    st <- get
    let s = st_installedPackages st
    put $ st { st_installedPackages = Set.insert pkg s }

addInstallablePackage :: PkgName -> Hkg ()
addInstallablePackage pkg = do
    st <- get
    let s = st_installablePackages st
    put $ st { st_installablePackages = Set.insert pkg s }

getInstallablePackages :: Hkg [PkgName]
getInstallablePackages = do
    st <- get
    return $ Set.toList $ st_installablePackages st

getCommonDepInstallablePackages :: Hkg [PkgName]
getCommonDepInstallablePackages = do
    st <- get
    let isInstallable p = p `Set.member` st_installablePackages st
        commonDeps = Map.keys $ Map.filter (> 50) $ st_installCounts st
        commonDepsInstallable = filter isInstallable commonDeps
    return commonDepsInstallable

addNotInstallablePackage :: PkgName -> Hkg ()
addNotInstallablePackage pkg = do
    st <- get
    let s = st_notInstallablePackages st
    put $ st { st_notInstallablePackages = Set.insert pkg s }

addFailPackage :: PkgName -> Hkg ()
addFailPackage pkg = do
    st <- get
    let s = st_failPackages st
    put $ st { st_failPackages = Set.insert pkg s }

buildSucceeded :: PkgName -> Hkg ()
buildSucceeded pkg = do
    st <- get
    let s = st_buildablePackages st
    put $ st { st_buildablePackages = Set.insert pkg s }

buildFailed :: PkgName -> Hkg ()
buildFailed pkg = do
    st <- get
    let s = st_buildFailurePackages st
    put $ st { st_buildFailurePackages = Set.insert pkg s }

buildDepsFailed :: PkgName -> Hkg ()
buildDepsFailed pkg = do
    st <- get
    let s = st_buildDepFailurePackages st
    put $ st { st_buildDepFailurePackages = Set.insert pkg s }

dumpStats :: Int -> Hkg ()
dumpStats npkgs = do
    st <- get
    let fullHistogram = reverse $ sort $ map swap
                      $ Map.assocs $ st_installCounts st
        (manyHistogram, fewHistogram) = span ((>= 10) . fst) fullHistogram
        total = sum $ map fst fullHistogram
        summaryTable = [["Num packages",
                         show $ npkgs],
                        ["Installed packages",
                         show $ Set.size $ st_installedPackages st],
                        ["Installable packages",
                         show $ Set.size $ st_installablePackages st],
                        ["Not installable packages",
                         show $ Set.size $ st_notInstallablePackages st],
                        ["Failed packages",
                         show $ Set.size $ st_failPackages st],
                        ["Total reinstallations",
                         show total]]

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
                  (unlines $ Set.toList $ st_installedPackages st)
        writeFile (name </> "installable-packages")
                  (unlines $ Set.toList $ st_installablePackages st)
        writeFile (name </> "uninstallable-packages")
                  (unlines $ Set.toList $ st_notInstallablePackages st)
        writeFile (name </> "fail-packages")
                  (unlines $ Set.toList $ st_failPackages st)
        writeFile (name </> "install-counts")
                  (unlines $ map show $ Map.assocs $ st_installCounts st)

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
    liftIO $ writeFile (st_name st </> "buildable")
                       (unlines $ Set.toList $ st_buildablePackages st)
    liftIO $ writeFile (st_name st </> "buildFailed")
                       (unlines $ Set.toList $ st_buildFailurePackages st)
    liftIO $ writeFile (st_name st </> "buildDepsFailed")
                       (unlines $ Set.toList $ st_buildDepFailurePackages st)

