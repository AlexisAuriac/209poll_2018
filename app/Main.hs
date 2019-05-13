module Main where

import System.Exit
import Control.Exception

import ProcessArgv

poll :: IO ()
poll = do
    args <- processArgv
    print args

main :: IO ()
main = do
    res <- try poll :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
