
module Main (main) where

import Utils

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = do args <- getArgs
          case args of
              [name1, name2] ->
                  do let compName = "compare---" ++ name1 ++ "---" ++ name2
                     exists <- doesDirectoryExist compName
                     when exists $ die (show compName ++ " already exists")
                     createDirectory compName

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

                     writeFile (compName </> "buildable-buildable")     (unlines $ Set.toList ba1_ba2)
                     writeFile (compName </> "buildable-buildFailed")   (unlines $ Set.toList ba1_bF2)
                     writeFile (compName </> "buildable-depsFailed")    (unlines $ Set.toList ba1_dF2)
                     writeFile (compName </> "buildable-notTried")      (unlines $ Set.toList ba1_nt2)
                     writeFile (compName </> "buildFailed-buildable")   (unlines $ Set.toList bF1_ba2)
                     writeFile (compName </> "buildFailed-buildFailed") (unlines $ Set.toList bF1_bF2)
                     writeFile (compName </> "buildFailed-depsFailed")  (unlines $ Set.toList bF1_dF2)
                     writeFile (compName </> "buildFailed-notTried")    (unlines $ Set.toList bF1_nt2)
                     writeFile (compName </> "depsFailed-buildable")    (unlines $ Set.toList dF1_ba2)
                     writeFile (compName </> "depsFailed-buildFailed")  (unlines $ Set.toList dF1_bF2)
                     writeFile (compName </> "depsFailed-depsFailed")   (unlines $ Set.toList dF1_dF2)
                     writeFile (compName </> "depsFailed-notTried")     (unlines $ Set.toList dF1_nt2)
                     writeFile (compName </> "notTried-buildable")      (unlines $ Set.toList nt1_ba2)
                     writeFile (compName </> "notTried-buildFailed")    (unlines $ Set.toList nt1_bF2)
                     writeFile (compName </> "notTried-depsFailed")     (unlines $ Set.toList nt1_dF2)
                     writeFile (compName </> "notTried-notTried")       (unlines $ Set.toList nt1_nt2)
                     writeFile (compName </> "summary")                 (unlines $ showTable padders table)

                     mapM_ putStrLn $ showTable padders table
              _ ->
                  die "Bad args"

type PkgName = String

readPkgList :: FilePath -> IO (Set PkgName)
readPkgList fp = do xs <- readFile fp
                    return $ Set.fromList $ lines xs

die :: String -> IO a
die err = do hPutStrLn stderr err
             exitWith (ExitFailure 1)


