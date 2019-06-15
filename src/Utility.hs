module Utility (
    allElemsOf,
    isUInt,
    isInt,
    isUFloat,
    isFloat
) where

allElemsOf :: (Eq a) => [a] -> [a] -> Bool
allElemsOf xs src = all (`elem` src) xs

isUInt :: String -> Bool
isUInt "" = False
isUInt xs = allElemsOf xs "0123456789"

isInt :: String -> Bool
isInt ('-':xs) = isUInt xs
isInt xs = isUInt xs

isUFloat :: String -> Bool
isUFloat "" = False
isUFloat ('.':_) = False
isUFloat xs = isUFloat' xs

isUFloat' :: String -> Bool
isUFloat' "" = True
isUFloat' ('.':xs) = isUInt xs
isUFloat' (x:xs)
    | x `elem` "0123456789" = isUFloat' xs
    | otherwise = False

isFloat :: String -> Bool
isFloat ('-':xs) = isUFloat xs
isFloat xs = isUFloat xs
