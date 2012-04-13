-- | Handle building a set of packages (usually all of Hackage)
module BuildManager (
        getPackages,
        tryBuildingPackages
    ) where

import Build
import BuildTools
import HackageMonad
import Utils

import Control.Monad.State
import Data.List
import Prelude hiding (catch)

-- The build is faster if we only build common dependencies once, but currently
-- the code assumes that all of them will install. As this isn't the case, turn
-- it off.
buildCommonPackagesOnce :: Bool
buildCommonPackagesOnce = False

-- | Should we only collect basic package statistics and not acutally build.
statsOnly :: Bool
statsOnly = False

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
tryBuildingPackages :: [PkgName] -> Hkg ()
tryBuildingPackages ps = do
    -- Our main objective here is to find out how many times each package would
    -- be installed as a dependency of another package.
    zipWithM_ (statPkg $ length ps) ps [1..]
    info ""
    info ""
    -- Write out the data so we can look at it (by hand) later if we want.
    dumpStats (length ps)
    unless statsOnly $ do
        commonPackageConf <- getCommonPackageConf
        initialisePackageConf commonPackageConf
        if buildCommonPackagesOnce
            then do
                info "Testing common dependency packages"
                psCommon <- getCommonDepInstallablePackages
                zipWithM_ (buildPkg $ length psCommon) psCommon [1..]
                info "Building common dependency packages"
                buildInCommon psCommon
                info "Testing remaining dependency packages"
                psAll <- getInstallablePackages
                let psUncommon = psAll \\ psCommon
                zipWithM_ (buildPkg $ length psUncommon) psUncommon [1..]

            else do
                psAll <- getInstallablePackages
                zipWithM_ (buildPkg $ length psAll) psAll [1..]

        dumpResults

