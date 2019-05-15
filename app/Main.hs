module Main where

import System.Exit
import Control.Exception
import Text.Printf

import ProcessArgv

poll :: IO ()
poll = do
    args <- processArgv
    printf "Population size:\t%d\n" (argSizeP args)
    printf "Sample size:\t\t%d\n" (argSizeS args)
    printf "Voting intentions:\t%.2f%%\n" (argP args)

main :: IO ()
main = do
    res <- try poll :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
