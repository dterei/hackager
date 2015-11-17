-- | Parse all the option flags for the record command.
module RecordOptions (
        recordHelp,
        processArgs
    ) where

import Control.Monad.State
import Data.Char (isDigit)
import System.Directory
import System.Exit (exitWith, ExitCode(..))
import System.IO

import HackageMonad

-- | Print usage information and exit
recordHelp :: ExitCode -> IO ()
recordHelp exitCode = do
    let out = case exitCode of
                    ExitSuccess   -> putStrLn
                    ExitFailure 1 -> putStrLn
                    ExitFailure _ -> hPutStrLn stderr
    mapM_ out
        [ "usage: hackager record -o NAME [-c CABAL] [-g GHC] [-p GHC-PKG] [-a CABAL-FLAGS]"
        , "                               [-d DEP-FLAGS] [-f PKG-FLAGS] [-n THREADS]"
        , "                               [-r SEARCH] [PKGS...]"
        , ""
        , "    NAME         A name by which the results of this hackager run will"
        , "                 be referred, e.g. \"ghc-6.12.1\""
        , "    CABAL        The path to the cabal program to use"
        , "    GHC          The path to the ghc program to use"
        , "    GHC-PKG      The path to the ghc-pkg program to use"
        , "    CABAL-FLAGS  Flags to pass to cabal during building"
        , "                 e.g. -a \"[\\\"--ghc-option=-XFoo\\\",\\\"--ghc-option=-XBar\\\"]\""
        , "    DEP-FLAGS    The flags to use when compiling dependencies of a package"
        , "                 e.g. -d \"[\\\"--ghc-option=-XFoo\\\",\\\"--ghc-option=-XBar\\\"]\""
        , "    PKG-FLAGS    The flags to use when compiling a package"
        , "                 e.g. -f [\"--ghc-option=-XFoo\",\"--ghc-option=-XBar\"]"
        , "    THREADS      Number of threads to use to build in parallel"
        , "    SEARCH       A regular expression to use for selecting packages,"
        , "                 when used, don't specify a package list"
        , "    PKGS         A list of packages to build. If not specified all of"
        , "                 hackage is built"
        ]
    exitWith exitCode

-- | Parse all the option flags for the record command.
processArgs :: [String] -> Hkg ()
processArgs []               = validateFlags
processArgs (('-':x):y:args) = processOpt x y >> processArgs args
processArgs args             = parsePackages args >> validateFlags

-- | Parse an individual option flag
processOpt :: String -> String -> Hkg ()
processOpt "o" name = do
    checkNotSet getRunPath "output directory is already set"
    checkNotOption name "the output directory is invalid"
    exists <- liftIO $ doesDirectoryExist name
    when exists $ die "The specified output directory already exists"
    setRunPath name

processOpt "c" cabal = do
    checkNotSet getCabal "cabal program is already set"
    checkNotOption cabal "cabal program is invalid"
    checkExecutable cabal "cabal"
    setCabal cabal

processOpt "g" ghc = do
    checkNotSet getGhc "ghc program is already set"
    checkNotOption ghc "ghc program is invalid"
    checkExecutable ghc "ghc"
    setGhc ghc

processOpt "a" cflags = do
    checkNotSet getCabalFlags "cabal flag already set"
    setCabalFlags cflags

processOpt "p" ghcpkg = do
    checkNotSet getGhcPkg "ghc-pkg program is already set"
    checkNotOption ghcpkg "ghc-pkg is invalid"
    checkExecutable ghcpkg "ghc-pkg"
    setGhcPkg ghcpkg

processOpt "d" depflags = do
    checkNotSet getDepFlags "dependency flag already set"
    setDepFlags depflags

processOpt "f" pkgflags = do
    checkNotSet getPkgFlags "package flag already set"
    setPkgFlags pkgflags

processOpt "n" threads = do
    let n = toInt threads
    case n of
        Just n' -> setThreads n'
        Nothing -> badflag "invalid thread number"

processOpt "r" regex = do
    checkNotSet getRegex "regex flag already set"
    setRegex regex

processOpt o _ = badflag $ "Unknown option '-" ++ o ++ "'"

-- | Parse the package list at the end
parsePackages :: [String] -> Hkg ()
parsePackages []     = return ()
parsePackages (x:xs) = do
    checkNotOption x $ "package '" ++ x ++ "' is not a valid package name"
    addPkg x
    parsePackages xs

-- | Validate all the flags needed have been set and are valid
validateFlags :: Hkg ()
validateFlags = do
    n <- getRunPath
    when (n == "") $ badflag "output directory not set"
    setExecutable getCabal setCabal "cabal"
    setExecutable getGhc setGhc "ghc"
    setExecutable getGhcPkg setGhcPkg "ghc-pkg"

-- | Set the executable to what's on the PATH if not set
setExecutable :: Hkg FilePath -> (FilePath -> Hkg ()) -> String -> Hkg ()
setExecutable getx setx name = do
    x <- getx
    when (x == "") $ do
        ci <- liftIO $ findExecutable name
        case ci of
            Nothing  -> badflag $ "can't find " ++ name ++ " executable"
            Just ci' -> setx ci'

-- | Make sure a file exists and is executable
checkExecutable :: String -> String -> Hkg ()
checkExecutable f prog = do
    b <- liftIO $ doesFileExist f
    unless b $ badflag $ prog ++ " executable doesn't exist"
    p <- liftIO $ getPermissions f
    unless (executable p) $ badflag $ prog ++ " file is not executable"

-- | Make sure a flag hasn't been set before
checkNotSet :: Hkg [a] -> String -> Hkg ()
checkNotSet getter errmsg = do
    val <- getter
    case val of
        [] -> return ()
        _  -> badflag errmsg

-- | Check that a value isn't an option
checkNotOption :: String -> String -> Hkg ()
checkNotOption ('-':_) errmsg = badflag errmsg
checkNotOption _ _            = return ()

-- | Parse a string to an int
toInt :: String -> Maybe Int
toInt str | all isDigit str = Just $ read str
          | otherwise       = Nothing

-- | Throw an error message in the face of a bad flag
badflag :: String -> Hkg ()
badflag errmsg = liftIO $ do
    hPutStrLn stderr errmsg
    recordHelp (ExitFailure 129)

