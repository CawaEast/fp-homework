module A5MonoidsSpec where

import           Data.Either    (either)
import           Data.Maybe     (fromMaybe)
import           TreePrinters   (Tree (..))
import           Data.Semigroup (Semigroup (..))
import           A5Monoids
import           A3Types(fromList)
import           A4Fold(toList)
import           Data.List(sort)
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
         runIdentity (Identity (Sum 4) <> Identity mempty) `shouldBe` runIdentity(Identity (Sum 4))
         runIdentity (Identity "Hello " <> Identity "World!") `shouldBe`  runIdentity (Identity "Hello World!")
         runIdentity (Identity mempty <> Identity "World!") `shouldBe`  runIdentity (Identity "World!")
         runIdentity (Identity [1, 2, 3] <> Identity [4, 5, 6]) `shouldBe`  runIdentity (Identity [1 .. 6])
         runIdentity (Identity [1, 2, 3] <> Identity mempty) `shouldBe`  runIdentity (Identity [1 .. 3])
          
    it "Name" $ do
         (Name "www" <> Name "google" <> Name "com") == (Name "www.google.com") `shouldBe` True
         (Name "sobaka" <> Name "ru") == (Name "sobaka.ru") `shouldBe` True
         (Name mempty <> Name "google" <> Name mempty) == (Name ".google.") `shouldBe` True
         (mempty `mappend` Name "google" `mappend` mempty) == (Name "google") `shouldBe` True
         (Name "www" <> Name "kgeorgiy" <> Name "org") == (Name "www.kgeorgiy.org") `shouldBe` True
    
    it "Endo" $ do
         (getEndo (Endo (\a -> a + 2) <> Endo (\a -> a + 5))) (Sum 3) `shouldBe` (\x -> x + 7) (Sum 3)
         (getEndo (Endo (\a -> a ++ ",hi") <> Endo (\a -> a ++ "!"))) "Bob" `shouldBe` (\x -> x ++ ",hi!") "Bob"
         (getEndo (Endo (\a -> a + 2) <> Endo (\a -> a - 3))) (Sum 3) `shouldBe` (\x -> x - 1) (Sum 3)
         (getEndo (mempty <> Endo (\a -> a - 3))) (Sum 3) `shouldBe` (\x -> x - 3) (Sum 3)
         
    it "Arror" $ do
         (getArrow (Arrow (\a -> a + 2) <> Arrow (\a -> a - 3))) (Sum 3) `shouldBe` (Sum 5)
         (getArrow (Arrow (\a -> Sum (length a)) <> Arrow (\a ->  Sum (head a)))) [1, 2, 3] `shouldBe` (Sum 4)
         
    it "treeMon" $ do
         toList ((fromList [1..10]) <> (fromList [5 .. 15])) `shouldBe` [1..15]
         toList (mempty `mappend` (fromList [5 .. 15])) `shouldBe` [5..15]
         toList ((fromList "qwerty") `mappend` (fromList "wasdf")) `shouldBe` sort "qwertyasdf"
           
         