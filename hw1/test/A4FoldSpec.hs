module A4FoldSpec where

import           A3Types           (fromList)
import           A4Fold
import           Control.Exception (evaluate)
import           Test.Hspec
import           Tree              (Tree (..))

spec :: Spec
spec = do
    it "foldMapTree" $ do
        foldMap (:[]) (fromList [3..8])       `shouldBe` [3..8]
        foldMap (:[]) (fromList [1])          `shouldBe` [1]
        foldMap (:[]) (fromList [8, 2, 5, 6]) `shouldBe` [2, 5, 6, 8]

    it "foldrTree" $ do
        sum (fromList [3..8])       `shouldBe` sum [3..8]
        sum (fromList [1])          `shouldBe` sum [1]
        sum (fromList [8, 2, 5, 6]) `shouldBe` sum [2, 5, 6, 8]

    it "toListTree" $ do
        toList (fromList [3..8])       `shouldBe` [3..8]
        toList (fromList [1])          `shouldBe` [1]
        toList (fromList [8, 2, 5, 6]) `shouldBe` [2, 5, 6, 8]

    it "splitOn" $ do
        splitOn '/' "path/to/file"   `shouldBe` ["path", "to", "file"]
        splitOn '/' "/"              `shouldBe` [[],[]]
        splitOn '/' "/path/to/file/" `shouldBe` [[], "path", "to", "file", []]

    it "joinWith" $ do
        joinWith '/' ["path", "to", "file"]         `shouldBe` "path/to/file"
        joinWith '/' [[],[]]                        `shouldBe` "/"
        joinWith '/' [[], "path", "to", "file", []] `shouldBe` "/path/to/file/"
