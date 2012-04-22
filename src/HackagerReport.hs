-- | Reporting tool. Perform a comparision of two Hackage Test runs.
module Main (main) where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import HackageMonad (PkgName)
import Utils

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name1, name2] -> report name1 name2
        ["-h"    ] -> usageInfo ExitSuccess
        ["--help"] -> usageInfo ExitSuccess
        ["help"  ] -> usageInfo ExitSuccess
        [        ] -> usageInfo ExitSuccess
        _          -> usageInfo (ExitFailure 1)

-- | Print usage information and exit
usageInfo :: ExitCode -> IO ()
usageInfo exitCode = do
    p <- getProgName
    mapM_ putStrLn
        [ "Usage: " ++ p ++  "<name1> <name2>"
        , "  name1:    The name of the first result"
        , "  name2:    The name of the second result"
        , "The two results are then compared with each other, the output"
        , "is placed in the folder 'compare---<name1>---<name2>'"
        ]
    exitWith exitCode

-- | Generate a Hackager comparison report
report :: String -> String -> IO ()
report name1 name2 = do
    let compName = "compare---" ++ name1 ++ "---" ++ name2
    exists <- doesDirectoryExist compName
    when exists $ die (show compName ++ " already exists")

    ba1 <- readPkgList (name1 </> "buildable")
    ba2 <- readPkgList (name2 </> "buildable")
    bF1 <- readPkgList (name1 </> "buildFailed")
    bF2 <- readPkgList (name2 </> "buildFailed")
    dF1 <- readPkgList (name1 </> "buildDepsFailed")
    dF2 <- readPkgList (name2 </> "buildDepsFailed")

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
        padders = [rpad,  rpad,           lpad,         lpad,           lpad,          lpad]
        table =  [["",    "",             name1,        "",             "",            ""],
                  ["",    "",             "Buildable",  "Build failed", "Deps failed", "Not tried"],
                  [name2, "Buildable",    num ba1_ba2,  num bF1_ba2,    num dF1_ba2,   num nt1_ba2],
                  ["",    "Build failed", num ba1_bF2,  num bF1_bF2,    num dF1_bF2,   num nt1_bF2],
                  ["",    "Deps failed",  num ba1_dF2,  num bF1_dF2,    num dF1_dF2,   num nt1_dF2],
                  ["",    "Not tried",    num ba1_nt2,  num bF1_nt2,    num dF1_nt2,   num nt1_nt2]]

    createDirectory compName
    mapM_ (writeResultFile compName)
        [ ("buildable-buildable"     , ba1_ba2)
        , ("buildable-buildFailed"   , ba1_bF2)
        , ("buildable-depsFailed"    , ba1_dF2)
        , ("buildable-notTried"      , ba1_nt2)
        , ("buildFailed-buildable"   , bF1_ba2)
        , ("buildFailed-buildFailed" , bF1_bF2)
        , ("buildFailed-depsFailed"  , bF1_dF2)
        , ("buildFailed-notTried"    , bF1_nt2)
        , ("depsFailed-buildable"    , dF1_ba2)
        , ("depsFailed-buildFailed"  , dF1_bF2)
        , ("depsFailed-depsFailed"   , dF1_dF2)
        , ("depsFailed-notTried"     , dF1_nt2)
        , ("notTried-buildable"      , nt1_ba2)
        , ("notTried-buildFailed"    , nt1_bF2)
        , ("notTried-depsFailed"     , nt1_dF2)
        , ("notTried-notTried"       , nt1_nt2)
        ]
    writeFile (compName </> "summary") (unlines $ showTable padders table)
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

