module A2ListsSpec where

import           A2Lists
import           Control.Exception (evaluate)
import           Test.Hspec

spec :: Spec
spec = do
    it "removeAt" $ do
        removeAt 1 [1,2,3] `shouldBe` [1,3]
        removeAt 10 [1,2,3] `shouldBe` [1,2,3]
        removeAt 3 [1..5] `shouldBe` [1,2,3,5]
        removeAt 2 "abc" `shouldBe` "ab"
        removeAt 0 "abc" `shouldBe` "bc"

    it "removeAtHard" $ do
        removeAtHard 1 [1,2,3] `shouldBe` (Just 2, [1,3])
        removeAtHard 10 [1,2,3] `shouldBe` (Nothing, [1,2,3])
        removeAtHard 3 [1..5] `shouldBe` (Just 4, [1,2,3,5])
        removeAtHard 2 "abc" `shouldBe` (Just 'c', "ab")
        removeAtHard 0 "abc" `shouldBe` (Just 'a', "bc")

    it "collectEvery" $ do
        collectEvery 3 [1..8] `shouldBe` ([1,2,4,5,7,8],[3,6])
        collectEvery 2 [1..8] `shouldBe` ([1,3,5,7], [2,4,6,8])
        collectEvery 1 [1..8] `shouldBe` ([], [1,2,3,4,5,6,7,8])
        collectEvery 10 [1..8] `shouldBe` ([1,2,3,4,5,6,7,8], [])

    it "stringSumGood" $ do
        stringSum "1 1" `shouldBe` 2
        stringSum "100\n\t-3" `shouldBe` 97
        stringSum "1" `shouldBe` 1
        stringSum "1 " `shouldBe` 1
        stringSum " 1" `shouldBe` 1
        stringSum "1 2 3" `shouldBe` 6
        stringSum "\t1\t" `shouldBe` 1
        stringSum "\t12345\t" `shouldBe` 12345
        stringSum "010 020 030" `shouldBe` 60
        stringSum "-1" `shouldBe` -1
        stringSum "-1 -2 -3" `shouldBe` -6
        stringSum "\t-12345\t" `shouldBe` -12345
        stringSum "\n1\t\n3   555  -1\n\n\n-5" `shouldBe` 553
        stringSum "123\t\n\t\n\t\n321 -4 -40" `shouldBe` 400
    it "stringSumBad" $ do
        evaluate (stringSum "asd") `shouldThrow` anyException
        evaluate (stringSum "1-1") `shouldThrow` anyException
        evaluate (stringSum "1.2") `shouldThrow` anyException
        evaluate (stringSum "--2") `shouldThrow` anyException
        evaluate (stringSum "+1") `shouldThrow` anyException
        evaluate (stringSum "1+") `shouldThrow` anyException
    it "stringSumHardGood" $ do
        stringSumHard "+1" `shouldBe` 1
        stringSumHard "1 +1" `shouldBe` 2
        stringSumHard "-1 +1" `shouldBe` 0
        stringSumHard "+1 -1" `shouldBe` 0
    it "stringSumHardBad" $ do
        evaluate (stringSumHard "1+1") `shouldThrow` anyException
        evaluate (stringSumHard "++1") `shouldThrow` anyException
        evaluate (stringSumHard "-+1") `shouldThrow` anyException
        evaluate (stringSumHard "+-1") `shouldThrow` anyException
        evaluate (stringSumHard "1 + 1") `shouldThrow` anyException

    it "mergeSort" $ do
        mergeSort [2, 1, 0, 3, 10, 5] `shouldBe` [0, 1, 2, 3, 5, 10]
        mergeSort [1, 2, 3] `shouldBe` [1, 2, 3]
--        (mergeSort []) `shouldBe` []
        mergeSort [1] `shouldBe` [1]
        mergeSort [1, 1, 1] `shouldBe` [1, 1, 1]
        mergeSort [6, 5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5, 6]
