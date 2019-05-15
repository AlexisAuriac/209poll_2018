module Main where

import System.Exit
import Control.Exception
import Text.Printf

import ProcessArgv
import Interval

calcVariance :: Int -> Int -> Float -> Float
calcVariance pop sample p = (((1-p)*p)/fSample) * ((fPop-fSample)/(fPop-1))
    where
        fSample = fromIntegral sample :: Float
        fPop = fromIntegral pop :: Float

poll :: IO ()
poll = do
    (pop, sample, p) <- processArgv
    let var = calcVariance pop sample p
    let inter95 = calcInterval p var 1.96
    let inter99 = calcInterval p var 2.58

    printf "Population size:\t\t%d\n" pop
    printf "Sample size:\t\t\t%d\n" sample
    printf "Voting intentions:\t\t%.2f%%\n" (p*100)
    printf "Variance:\t\t\t%.6f\n" var
    displayInterval 95 inter95
    displayInterval 99 inter99

main :: IO ()
main = do
    res <- try poll :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
