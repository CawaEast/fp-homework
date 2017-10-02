module A5MonoidsSpec where

import           A3Types           (fromList)
import           A4Fold            (toList)
import           A5Monoids
import           Control.Exception (evaluate)
import           Data.Either       (either)
import           Data.List         (sort)
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       (Product (..), Sum (..))
import           Data.Semigroup    (Semigroup (..))
import           Test.Hspec
import           Tree              (Tree (..))

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
        runIdentity (Identity (Sum 4) <> Identity (Sum 5)) `shouldBe` Sum 9
        runIdentity (Identity (Sum 4) <> Identity mempty) `shouldBe` Sum 4
        runIdentity (Identity "Hello " <> Identity "World!") `shouldBe` "Hello World!"
        runIdentity (Identity mempty <> Identity "World!") `shouldBe` "World!"
        runIdentity (Identity [1, 2, 3] <> Identity [4, 5, 6]) `shouldBe` [1 .. 6]
        runIdentity (Identity [1, 2, 3] <> Identity mempty) `shouldBe` [1 .. 3]

    it "Name" $ do
        (Name "www" <> Name "google" <> Name "com") == Name "www.google.com" `shouldBe` True
        (Name "sobaka" <> Name "ru") == Name "sobaka.ru" `shouldBe` True
        (Name mempty <> Name "google" <> Name mempty) == Name ".google." `shouldBe` True
        (mempty `mappend` Name "google" `mappend` mempty) == Name "google" `shouldBe` True
        (Name "www" <> Name "kgeorgiy" <> Name "org") == Name "www.kgeorgiy.org" `shouldBe` True

    it "Endo" $ do
        getEndo (Endo (+ 2) <> Endo (+ 5)) (Sum 3) `shouldBe` Sum 10
        getEndo (Endo (++ ",hi") <> Endo (++ "!")) "Bob" `shouldBe` "Bob,hi!"
        getEndo (Endo (+ 2) <> Endo (+ (-3))) (Sum 3) `shouldBe` Sum 2
        getEndo (mempty <> Endo (+ (-3))) (Sum 3) `shouldBe` Sum 0

    it "Arror" $ do
        getArrow (Arrow (+ 2) <> Arrow (+ (- 3))) (Sum 3) `shouldBe` Sum 5
        getArrow (Arrow (Sum . length) <> Arrow (Sum . head)) [1, 2, 3] `shouldBe` Sum 4
        getArrow (Arrow (Product . length) <> Arrow (Product . head)) [4, 2, 3] `shouldBe` Product 12

    it "treeMon" $ do
        toList (fromList [1..10] <> fromList [5 .. 15]) `shouldBe` [1..15]
        toList (mempty `mappend` fromList [5 .. 15]) `shouldBe` [5..15]
        toList (fromList "qwerty" `mappend` fromList "wasdf") `shouldBe` sort "qwertyasdf"
