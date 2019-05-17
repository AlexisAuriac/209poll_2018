module UtilityTest (
    testUtility
) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Utility

testAllElemsOf :: IO ()
testAllElemsOf = hspec $ do
    describe "Utility.allElemsOf" $ do
        it "returns True if the list is empty" $ do
            allElemsOf "" "any" `shouldBe` (True :: Bool)
            allElemsOf "" "" `shouldBe` (True :: Bool)

        it "returns True if the list is only composed of elements in the source" $ do
            allElemsOf "aaaaa" "abc" `shouldBe` (True :: Bool)
            allElemsOf "b" "abc" `shouldBe` (True :: Bool)
            allElemsOf "bca" "abc" `shouldBe` (True :: Bool)
            allElemsOf "c" "abc" `shouldBe` (True :: Bool)
            allElemsOf "acbbabcbabcabcbabc" "abc" `shouldBe` (True :: Bool)
            allElemsOf [1,4,7,3,3,6,2,7,9,7,2,0] [0..9] `shouldBe` (True :: Bool)

        it "returns False if the list isn't only composed of elements in the source" $ do
            allElemsOf "aaaaad" "abc" `shouldBe` (False :: Bool)
            allElemsOf "d" "abc" `shouldBe` (False :: Bool)
            allElemsOf "a.bc" "abc" `shouldBe` (False :: Bool)
            allElemsOf "abcA" "abc" `shouldBe` (False :: Bool)
            allElemsOf "acbb0abcbabcabcbabc" "abc" `shouldBe` (False :: Bool)
            allElemsOf [10,4,7,3,3,6,2,7,9,7,2,0] [0..9] `shouldBe` (False :: Bool)

testIsInt :: IO ()
testIsInt = hspec $ do
    describe "Utility.isInt" $ do
        it "returns False if the string is empty" $ do
            isInt "" `shouldBe` (False :: Bool)

        it "returns False if the string contains any non-digit characters" $ do
            isInt "abc" `shouldBe` (False :: Bool)

        it "returns True if the string is composed only of digits" $ do
            isInt "123" `shouldBe` (True :: Bool)

        it "returns True if the string is composed only of digits and starts with -" $ do
            isInt "-123" `shouldBe` (True :: Bool)

testIsUInt :: IO ()
testIsUInt = hspec $ do
    describe "Utility.isUInt" $ do
        it "returns False if the string is empty" $ do
            isUInt "" `shouldBe` (False :: Bool)

        it "returns False if the string contains any non-digit characters" $ do
            isUInt "abc" `shouldBe` (False :: Bool)

        it "returns True if the string is composed only of digits" $ do
            isUInt "123" `shouldBe` (True :: Bool)

        it "returns False if the string is composed only of digits and starts with -" $ do
            isUInt "-123" `shouldBe` (False :: Bool)

testIsFloat :: IO ()
testIsFloat = hspec $ do
    describe "Utility.isFloat" $ do
        it "returns False if the string is empty" $ do
            isFloat "" `shouldBe` (False :: Bool)

        it "returns False if the string contains any non-digit characters" $ do
            isFloat "abc" `shouldBe` (False :: Bool)

        it "returns True if the string is composed only of digits" $ do
            isFloat "123" `shouldBe` (True :: Bool)

        it "returns True if the string is composed only of digits and starts with -" $ do
            isFloat "-123" `shouldBe` (True :: Bool)

        it "returns True if the string has a dot '.' in it (but not at the start or the end)" $ do
            isFloat ".123" `shouldBe` (False :: Bool)
            isFloat "-.123" `shouldBe` (False :: Bool)
            isFloat "123." `shouldBe` (False :: Bool)
            isFloat "-123." `shouldBe` (False :: Bool)
            isFloat "1.23" `shouldBe` (True :: Bool)
            isFloat "-1.23" `shouldBe` (True :: Bool)
            isFloat "12.3" `shouldBe` (True :: Bool)
            isFloat "-12.3" `shouldBe` (True :: Bool)
            isFloat "1.2.3" `shouldBe` (False :: Bool)
            isFloat "-1.2.3" `shouldBe` (False :: Bool)


testIsUFloat :: IO ()
testIsUFloat = hspec $ do
    describe "Utility.isUFloat" $ do
        it "returns False if the string is empty" $ do
            isUFloat "" `shouldBe` (False :: Bool)

        it "returns False if the string contains any non-digit characters" $ do
            isUFloat "abc" `shouldBe` (False :: Bool)

        it "returns True if the string is composed only of digits" $ do
            isUFloat "123" `shouldBe` (True :: Bool)

        it "returns False if the string is composed only of digits and starts with -" $ do
            isUFloat "-123" `shouldBe` (False :: Bool)

        it "returns True if the string has a dot '.' in it (but not at the start or the end)" $ do
            isUFloat ".123" `shouldBe` (False :: Bool)
            isUFloat "123." `shouldBe` (False :: Bool)
            isUFloat "1.23" `shouldBe` (True :: Bool)
            isUFloat "12.3" `shouldBe` (True :: Bool)
            isUFloat "1.2.3" `shouldBe` (False :: Bool)

testUtility :: IO ()
testUtility = do
    testAllElemsOf
    testIsUInt
    testIsInt
    testIsFloat
    testIsUFloat
