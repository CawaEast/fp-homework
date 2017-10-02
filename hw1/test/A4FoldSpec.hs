module A4FoldSpec where

import           A4Fold
import           A3Types(fromList)
import           Test.Hspec
import           Control.Exception (evaluate)
import           TreePrinters (Tree (..))

spec :: Spec
spec = do 
    it "foldMapTree" $ do
         foldMap (:[]) (fromList [3..8]) `shouldBe` [3..8]
         foldMap (:[]) (fromList [1]) `shouldBe` [1]
         foldMap (:[]) (fromList [8, 2, 5, 6]) `shouldBe` [2, 5, 6, 8]
    
    it "foldrTree" $ do
         foldr (+) 0 (fromList [3..8]) `shouldBe` sum [3..8]
         foldr (+) 0 (fromList [1]) `shouldBe` sum [1]
         foldr (+) 0 (fromList [8, 2, 5, 6]) `shouldBe` sum [2, 5, 6, 8]
         
    it "toListTree" $ do
         toList (fromList [3..8]) `shouldBe` [3..8]
         toList (fromList [1]) `shouldBe` [1]
         toList (fromList [8, 2, 5, 6]) `shouldBe` [2, 5, 6, 8]
         
    it "splitOn" $ do
         splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
         splitOn '/' "/" `shouldBe` [[],[]]
         splitOn '/' "/path/to/file/" `shouldBe` [[], "path", "to", "file", []]
    
    it "joinWith" $ do
         joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
         joinWith '/' [[],[]] `shouldBe` "/"
         joinWith '/' [[], "path", "to", "file", []] `shouldBe` "/path/to/file/"