-- | Utility functions.
module Utils (
        catchIO, ignoreException, onEndOfFile,
        showTable, uniq, swap, lpad, rpad
    ) where

import Control.Exception
import Data.List
import Prelude hiding (catch)
import System.IO.Error hiding (catch)

-- | Handle an IO exception.
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

-- | Ignore any IO exception that arises.
ignoreException :: IO () -> IO ()
ignoreException io = io `catchIO` \_ -> return ()

-- | Perform some IO and execute the second argument on an EOF exception.
onEndOfFile :: IO a -> IO a -> IO a
onEndOfFile io io' =
    io `catch` \e -> if isEOFError e then io' else throwIO e

-- | Filter out adjacent duplicate elements.
uniq :: Eq a => [a] -> [a]
uniq (x : y : xs)
  | x == y    = uniq (x : xs)
uniq (x : xs) = x : uniq xs
uniq []       = []

-- | Swap elements of a tuple.
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- | Show output in a tabular format.
showTable :: [Int -> String -> String] -> [[String]] -> [String]
showTable padders xss =
    let lengths = map (maximum . map length) $ transpose xss
    in map (unwords . zipWith3 id padders lengths) xss

-- | Pad the string with spaces before it.
lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

-- | Pad the string with spaces after it.
rpad :: Int -> String -> String
rpad n s = s ++ replicate (n - length s) ' '

