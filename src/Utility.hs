module Utility (
    allElemsOf,
    isUInt,
    isInt,
    isUFloat,
    isFloat,
    printList
) where

allElemsOf :: (Eq a) => [a] -> [a] -> Bool
allElemsOf xs src = and inSrc
    where inSrc = map (`elem` src) xs

isUInt :: String -> Bool
isUInt "" = False
isUInt xs = allElemsOf xs "0123456789"

isInt :: String -> Bool
isInt "" = False
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
isFloat "" = False
isFloat ('-':xs) = isUFloat xs
isFloat xs = isUFloat xs

printList :: (Show a) => [a] -> (a -> IO ()) -> IO ()
printList [] _ = return ()
printList (x:xs) f = do
    f x
    printList xs f
