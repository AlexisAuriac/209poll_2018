module Interval (
    calcInterval,
    displayInterval
) where

import Text.Printf

calcInterval :: Float -> Float -> Float -> (Float, Float)
calcInterval p var mul = (minVal, maxVal)
    where
        diff = mul * (sqrt var)
        minVal = max (p - diff) 0
        maxVal = min (p + diff) 1

displayInterval :: Int -> (Float, Float) -> IO ()
displayInterval confidence (minVal, maxVal) =
    printf "%d%% confidence interval:\t[%.2f%%; %.2f%%]\n"
        confidence
        (minVal * 100)
        (maxVal * 100)
