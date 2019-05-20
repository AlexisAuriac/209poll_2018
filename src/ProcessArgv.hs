module ProcessArgv (
    processArgv
) where

import System.Environment

import Utility

type Arguments = (Int, Int, Float)

usage :: String -> String
usage progName = "Usage\n\
    \\t" ++ progName ++ " pSize sSize p\n\
    \\n\
    \DESCRIPTION\n\
    \\tpSize\tsize of the population\n\
    \\tsSize\tsize of the sample (supposed to be representative)\n\
    \\tp\tpercentage of voting intentions for a specific candidate"

processArgv :: IO Arguments
processArgv = do
    argv <- getArgs
    progName <- getProgName
    return $ processArgv' argv progName
processArgv' :: [String] -> String -> Arguments

processArgv' argv progName
    | length argv /= 3 = error $ usage progName
    | (not (isUInt (argv !! 0))) || sizeP == 0 = error "pSize must be an unsigned integer superior to 0"
    | (not (isUInt (argv !! 1))) || sizeS == 0 = error "sSize must be an unsigned integer superior to 0"
    | not $ isFloat (argv !! 2) = error "p must be a decimal number"
    | (p < 0) || (p > 100)  = error "p is a percentage, therefore it must be between 0 and 100"
    | (not (isFloat (argv !! 2))) || sizeS == 0 = error "sSize must an unsigned int superior to 0"
    | sizeS > sizeP = error "sSize must be inferior or equal to sizeP"
    | otherwise = (sizeP, sizeS, p/100)
    where
        sizeP = read (argv !! 0) :: Int
        sizeS = read (argv !! 1) :: Int
        p = read (argv !! 2) :: Float
