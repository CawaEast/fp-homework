module A1SimpleSpec where

import           A1Simple
import           Test.Hspec

spec :: Spec
spec = do
    it "order3" $ do
        order3 (1, 2, 3) `shouldBe` (1, 2, 3)
        order3 (3, 2, 1) `shouldBe` (1, 2, 3)
        order3 (10, 15, 5) `shouldBe` (5, 10, 15)
        order3 (1, 1, 1) `shouldBe` (1, 1, 1)
        order3 (4, 1, 3) `shouldBe` (1, 3, 4)

    it "highestBit" $ do
        highestBit 15 `shouldBe` 8
        highestBit 16 `shouldBe` 16
        highestBit 17 `shouldBe` 16
        highestBit 1 `shouldBe` 1
        highestBit 2 `shouldBe` 2

    it "highestBitHard" $ do
        highestBitHard 15 `shouldBe` (8, 3)
        highestBitHard 16 `shouldBe` (16, 4)
        highestBitHard 16 `shouldBe` (16, 4)
        highestBitHard 1 `shouldBe` (1, 0)
        highestBitHard 2 `shouldBe` (2, 1)

    it "smartReplicate" $ do
        smartReplicate [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
        smartReplicate [1, 1, 1] `shouldBe` [1, 1, 1]
        smartReplicate [5, 0] `shouldBe` [5, 5, 5, 5, 5]
        smartReplicate [] `shouldBe` []

    it "contains" $ do
        contains 3 [[1..5], [2,0], [3,4]] `shouldBe` [[1,2,3,4,5],[3,4]]
        contains 6 [[1..5], [2,0], [3,4]] `shouldBe` []
        contains 2 [[1..5], [2,0], [2,3,4]] `shouldBe` [[1,2,3,4,5],[2,0],[2,3,4]]
