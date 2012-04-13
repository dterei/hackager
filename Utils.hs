
module Utils where

import Control.Exception
import Data.List
import Prelude hiding (catch)
import System.IO.Error hiding (catch)

uniq :: Eq a => [a] -> [a]
uniq (x : y : xs)
 | x == y     = uniq (x : xs)
uniq (x : xs) = x : uniq xs
uniq []       = []

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

showTable :: [Int -> String -> String] -> [[String]] -> [String]
showTable padders xss
    = let lengths = map (maximum . map length) $ transpose xss
      in map (concat . intersperse " " . zipWith3 id padders lengths) xss

lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

rpad :: Int -> String -> String
rpad n s = s ++ replicate (n - length s) ' '

onEndOfFile :: IO a -> IO a -> IO a
onEndOfFile io io' = io `catch` \e -> if isEOFError e
                                      then io'
                                      else throwIO e

