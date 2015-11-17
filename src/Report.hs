-- | Reporting tool. Perform a comparision of two Hackage Test runs.
module Report (
        report,
        reportHelp
    ) where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.Exit (exitWith, ExitCode(..))
import System.FilePath
import System.IO

import HackageMonad (PkgName)
import Utils

-- | Run the reporting tool.
report :: [String] -> IO ()
report args =
    case args of
        [name1, name2] -> generate name1 name2
        ["--help"    ] -> reportHelp ExitSuccess
        [            ] -> reportHelp (ExitFailure 1)
        _              -> reportHelp (ExitFailure 1)

-- | Print usage information and exit
reportHelp :: ExitCode -> IO ()
reportHelp exitCode = do
    mapM_ putStrLn
        [ "usage: hackager report <name1> <name2>"
        , ""
        , "    name1    The name of the first result"
        , "    name2    The name of the second result"
        , ""
        , "The two results are then compared with each other, the output"
        , "is placed in the folder 'compare---<name1>---<name2>'"
        ]
    exitWith exitCode

-- | Generate a Hackager comparison report
generate :: String -> String -> IO ()
generate name1 name2 = do
    let compName = "compare---" ++ name1 ++ "---" ++ name2

    -- check valid input and outputs
    n1exists <- doesDirectoryExist name1
    unless n1exists $ die $ "'" ++ name1 ++ "' doesn't exists"
    n2exists <- doesDirectoryExist name2
    unless n2exists $ die $ "'" ++ name2 ++ "' doesn't exists"
    exists <- doesDirectoryExist compName
    when exists $ die $ "The directoy '" ++ compName ++
        "' already exists, won't overwrite"

    ba1 <- readPkgList $ name1 </> "build.success"
    ba2 <- readPkgList $ name2 </> "build.success"
    bF1 <- readPkgList $ name1 </> "build.fail"
    bF2 <- readPkgList $ name2 </> "build.fail"
    dF1 <- readPkgList $ name1 </> "build.depfail"
    dF2 <- readPkgList $ name2 </> "build.depfail"

    let all1 = ba1 `Set.union` bF1 `Set.union` dF1
        all2 = ba2 `Set.union` bF2 `Set.union` dF2
        -- Work out what was not tried by one build or the other
        nt1 = all2 `Set.difference` all1
        nt2 = all1 `Set.difference` all2

        ba1_ba2 = ba1 `Set.intersection` ba2
        ba1_bF2 = ba1 `Set.intersection` bF2
        ba1_dF2 = ba1 `Set.intersection` dF2
        ba1_nt2 = ba1 `Set.intersection` nt2
        bF1_ba2 = bF1 `Set.intersection` ba2
        bF1_bF2 = bF1 `Set.intersection` bF2
        bF1_dF2 = bF1 `Set.intersection` dF2
        bF1_nt2 = bF1 `Set.intersection` nt2
        dF1_ba2 = dF1 `Set.intersection` ba2
        dF1_bF2 = dF1 `Set.intersection` bF2
        dF1_dF2 = dF1 `Set.intersection` dF2
        dF1_nt2 = dF1 `Set.intersection` nt2
        nt1_ba2 = nt1 `Set.intersection` ba2
        nt1_bF2 = nt1 `Set.intersection` bF2
        nt1_dF2 = nt1 `Set.intersection` dF2
        nt1_nt2 = nt1 `Set.intersection` nt2

        num s = show (Set.size s)
        num' s = num s ++ " "
        padders = [rpad,  rpad,          lpad,         lpad,         lpad,           lpad]
        table =  [["",    "",            name1,        "",           "",             ""],
                  ["",    "",            "Built,",     "Failed,",    "Deps Failed,", "Not Tried"],
                  [name2, "Built",       num' ba1_ba2, num' bF1_ba2, num' dF1_ba2,   num nt1_ba2],
                  ["",    "Failed",      num' ba1_bF2, num' bF1_bF2, num' dF1_bF2,   num nt1_bF2],
                  ["",    "Deps Failed", num' ba1_dF2, num' bF1_dF2, num' dF1_dF2,   num nt1_dF2],
                  ["",    "Not Tried",   num' ba1_nt2, num' bF1_nt2, num' dF1_nt2,   num nt1_nt2]]

    createDirectory compName
    mapM_ (writeResultFile compName)
        [ ("built-built"      , ba1_ba2)
        , ("built-failed"     , ba1_bF2)
        , ("built-depfail"    , ba1_dF2)
        , ("built-nottried"   , ba1_nt2)
        , ("fail-built"       , bF1_ba2)
        , ("fail-failed"      , bF1_bF2)
        , ("fail-depfail"     , bF1_dF2)
        , ("fail-nottried"    , bF1_nt2)
        , ("depfail-built"    , dF1_ba2)
        , ("depfail-fail"     , dF1_bF2)
        , ("depfail-depfail"  , dF1_dF2)
        , ("depfail-nottried" , dF1_nt2)
        , ("nottried-built"   , nt1_ba2)
        , ("nottried-fail"    , nt1_bF2)
        , ("nottried-depfail" , nt1_dF2)
        , ("nottried-nottried", nt1_nt2)
        ]
    writeFile (compName </> "results") (unlines $ showTable padders table)
    mapM_ putStrLn $ showTable padders table

-- | Write results to a file
writeResultFile :: FilePath -> (FilePath, Set PkgName) -> IO ()
writeResultFile dir (f, set) = writeFile (dir </> f) (unlines $ Set.toList set)

-- | Read a list of packages from the file specified
readPkgList :: FilePath -> IO (Set PkgName)
readPkgList fp = do xs <- readFile fp
                    return $ Set.fromList $ lines xs

-- | Exit with an error
die :: String -> IO a
die err = do hPutStrLn stderr err
             exitWith (ExitFailure 1)

