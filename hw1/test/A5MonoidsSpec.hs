module A5MonoidsSpec where

import           Data.Either    (either)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup (..))
import           TreePrinters   (Tree (..))
import           A5Monoids
import           A3Types(fromList)
import           Test.Hspec
import           Control.Exception (evaluate)
import           Data.Monoid(Sum(..))

spec :: Spec
spec = do 
    it "maybeConcat" $ do
         maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1..5]
         --maybeConcat [Nothing, Nothing, Nothing] `shouldBe` []
         maybeConcat [Nothing, Just [3,2,1], Just [10,3]] `shouldBe` [3,2,1,10,3]
         
    it "eitherConcat" $ do
          eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] `shouldBe` (Sum {getSum = 8}, [1,2,3,4,5])
          eitherConcat [Left (Sum 3), Left (Sum 1), Left (Sum 5), Right [1]] `shouldBe` (Sum {getSum = 9}, [1])
          eitherConcat [Left (Sum 3), Right [4,5], Right [1,2,3], Left (Sum 5)] `shouldBe` (Sum {getSum = 8}, [4,5,1,2,3])
          
    it "nonEmpty" $ do
         (4:|[5..9]) <> (10 :|[11..20]) `shouldBe` 4:|[5..20]    
         ('a':|['b'..'f']) <> ('g':|['h'..'n']) `shouldBe` 'a':|['b'..'n']    
         ('a':|"bcdefghi") <> ('j':|"klmnopqr") `shouldBe` 'a':|['b' .. 'r']  
    
    it "Identity" $ do
         runIdentity (Identity (Sum 4) <> Identity (Sum 5)) `shouldBe` runIdentity(Identity (Sum 9))
         runIdentity (Identity "Hello " <> Identity "World!") `shouldBe`  runIdentity (Identity "Hello World!")
          
          