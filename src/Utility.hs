module Utility (
    allElemsOf,
    isUInt,
    isInt,
    isFloat,
    isUFloat,
    printList
) where

allElemsOf :: (Eq a) => [a] -> [a] -> Bool
allElemsOf [] _ = True
allElemsOf (c:xs) src = c `elem` src && (allElemsOf xs src)

isUInt :: String -> Bool
isUInt "" = False
isUInt xs = allElemsOf xs "0123456789"

isInt :: String -> Bool
isInt "" = False
isInt (x:xs)
    | x == '-' = allElemsOf xs digits
    | otherwise = allElemsOf (x:xs) digits
    where digits = "0123456789"

isFloat :: String -> Bool
isFloat "" = False
isFloat ('-':xs) = isUFloat xs
isFloat xs = isUFloat xs

isUFloat :: String -> Bool
isUFloat "" = False
isUFloat xs = isUFloat' xs

isUFloat' :: String -> Bool
isUFloat' "" = True
isUFloat' (x:xs)
    | x == '.' = isUInt xs
    | x `elem` "0123456789" = isUFloat' xs
    | otherwise = False

printList :: (Show a) => [a] -> (a -> IO ()) -> IO ()
printList [] _ = return ()
printList (x:xs) f = do
    f x
    printList xs f
