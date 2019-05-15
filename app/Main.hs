module Main where

import System.Exit
import Control.Exception
import Text.Printf

import ProcessArgv

calcVariance :: Int -> Int -> Float -> Float
calcVariance pop sample pPercent = (((1-p)*p)/fSample) * ((fPop-fSample)/(fPop-1))
    where
        fSample = fromIntegral sample :: Float
        fPop = fromIntegral pop :: Float
        p = pPercent / 100

poll :: IO ()
poll = do
    (pop, sample, p) <- processArgv
    printf "Population size:\t%d\n" pop
    printf "Sample size:\t\t%d\n" sample
    printf "Voting intentions:\t%.2f%%\n" p
    printf "Variance:\t\t%.6f\n" (calcVariance pop sample p)

main :: IO ()
main = do
    res <- try poll :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
