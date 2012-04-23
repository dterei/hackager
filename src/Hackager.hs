-- | Hackage Test. Build all of hackage.
module Main (main) where

import System.Environment
import System.Exit

import Record
import Report

-- | Hackage Test (IO)
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("record" : args') -> record args'
        ("report" : args') -> report args'
        ("help"   : args') -> help args'
        ["--version"] -> version
        ["--help"   ] -> usageInfo ExitSuccess
        [           ] -> usageInfo (ExitFailure 1)
        x:_           -> putStrLn ("hackager: '" ++ x ++ "' is not a hackager"
                            ++ " command. See 'hackager --help'.")
                         >> exitWith (ExitFailure 1)

-- | Hackager version
version :: IO ()
version = putStrLn "hackager version 1.0.0"

-- | The help command
help :: [String] -> IO ()
help [        ] = usageInfo ExitSuccess
help ["record"] = recordHelp ExitSuccess
help ["report"] = reportHelp ExitSuccess
help [x       ] = putStrLn ("Command '" ++ x ++  "' doesn't exist")
                  >> exitWith (ExitFailure 1)
help _          = usageInfo (ExitFailure 1)

-- | Print usage information and exit
usageInfo :: ExitCode -> IO ()
usageInfo exitCode = do
    mapM_ putStrLn
        [ "usage: hackager [--version] [--help] <command> [<args>]"
        , ""
        , "The valid hackager commands are:"
        , "    record    Try building all of hackage and record results"
        , "    report    Compare two 'record' runs and display results"
        , ""
        , "See 'hackager help <command>' for more information on a"
           ++ " specific command"
        ]
    exitWith exitCode

