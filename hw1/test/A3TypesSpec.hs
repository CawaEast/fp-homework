module A3TypesSpec where

import           A3Types
import           Control.Exception (evaluate)
import           Test.Hspec
import           Tree              (Tree (..))

spec :: Spec
spec = do
    it "nextDay" $ do
        nextDay Monday    `shouldBe` Tuesday
        nextDay Tuesday   `shouldBe` Wednesday
        nextDay Wednesday `shouldBe` Thursday
        nextDay Thursday  `shouldBe` Friday
        nextDay Friday    `shouldBe` Saturday
        nextDay Saturday  `shouldBe` Sunday
        nextDay Sunday    `shouldBe` Monday

    it "afterDays" $ do
        afterDays Monday 0    `shouldBe` Monday
        afterDays Monday 7    `shouldBe` Monday
        afterDays Monday 49   `shouldBe` Monday
        afterDays Sunday 365  `shouldBe` Monday
        afterDays Wednesday 4 `shouldBe` Sunday
        afterDays Friday 4    `shouldBe` Tuesday

    it "isWeekend" $ do
        isWeekend Monday    `shouldBe` False
        isWeekend Tuesday   `shouldBe` False
        isWeekend Wednesday `shouldBe` False
        isWeekend Thursday  `shouldBe` False
        isWeekend Friday    `shouldBe` False
        isWeekend Saturday  `shouldBe` True
        isWeekend Sunday    `shouldBe` True

    it "fight" $ do
        either name name (fst (fight (Knight  "Tom" 10 2) (Monster       10 1))) `shouldBe` "Tom"
        either name name (fst (fight (Knight  "Tom" 10 2) (Monster       10 3))) `shouldBe` "Name??? Just monster."
        either name name (fst (fight (Monster       10 2) (Knight  "Tom" 10 2))) `shouldBe` "Tom"
        either name name (fst (fight (Knight  "Sem" 10 2) (Knight  "Tom" 10 3))) `shouldBe` "Tom"
        either name name (fst (fight (Knight  "Sem" 10 3) (Knight  "Tom" 10 3))) `shouldBe` "Sem"
        either name name (fst (fight (Monster       10 1) (Monster       10 3))) `shouldBe` "Name??? Just monster."
        either ad   ad   (fst (fight (Monster       10 1) (Monster       10 3))) `shouldBe` 3

    it "vectorLength" $ do
        vectorLength (Vector2D 4  3)   `shouldBe` 5
        vectorLength (Vector2D 5  12)  `shouldBe` 13
        vectorLength (Vector2D 0  0)   `shouldBe` 0
        vectorLength (Vector3D 0  0 0) `shouldBe` 0
        vectorLength (Vector3D 1  1 1) `shouldBe` sqrt 3
        vectorLength (Vector3D 10 5 1) `shouldBe` sqrt 126

    it "vectorAdd" $ do
        (Vector2D 1    1    + Vector2D 2    2)     `shouldBe` Vector2D 3    3
        (Vector2D 1    2    + Vector2D 4    5)     `shouldBe` Vector2D 5    7
        (Vector2D (-5) 4    + Vector3D 2    2   5) `shouldBe` Vector3D (-3) 6 5
        (Vector3D 1    10 1 + Vector2D (-2) 2)     `shouldBe` Vector3D (-1) 12 1

    it "vectorScalar" $ do
        scalar (Vector2D 1 5)        (Vector2D 1 5)   `shouldBe` 26
        scalar (Vector2D 1 (-5))     (Vector2D 1 5)   `shouldBe` (-24)
        scalar (Vector3D 4 (-1) 999) (Vector2D 1 5)   `shouldBe` (-1)
        scalar (Vector3D 4 (-1) 9)   (Vector3D 2 5 1) `shouldBe` 12

    it "vectorDist" $ do
        dist (Vector2D 1 1)   (Vector2D 2 2)   `shouldBe` sqrt 2
        dist (Vector2D 1 2)   (Vector2D 1 5)   `shouldBe` 3
        dist (Vector3D 4 2 2) (Vector2D 1 5)   `shouldBe` sqrt 22
        dist (Vector3D 4 5 9) (Vector3D 4 5 1) `shouldBe` 8

    it "natToInt" $ do
        toInteger Z                     `shouldBe` 0
        toInteger (S Z)                 `shouldBe` 1
        toInteger (S (S Z))             `shouldBe` 2
        toInteger (S (S (S Z)))         `shouldBe` 3
        toInteger (S (S (S (S (S Z))))) `shouldBe` 5

    it "intRoNat" $ do
        (0 :: Nat)              `shouldBe` Z
        (1 :: Nat)              `shouldBe` S Z
        (2 :: Nat)              `shouldBe` S (S Z)
        (3 :: Nat)              `shouldBe` S (S (S Z))
        (5 :: Nat)              `shouldBe` S (S (S (S (S Z))))
        toInteger (100 :: Nat)  `shouldBe` 100


    it "natAdd" $ do
        Z           + Z           `shouldBe` Z
        S Z         + S (S Z)     `shouldBe` S (S (S Z))
        S (S (S Z)) + S (S Z)     `shouldBe` S (S (S (S (S Z))))
        (10 :: Nat) + (10 :: Nat) `shouldBe` (20 :: Nat)

    it "natMul" $ do
        Z           * (10 :: Nat) `shouldBe` Z
        S Z         * S (S Z)     `shouldBe` S (S Z)
        S (S (S Z)) * S (S Z)     `shouldBe` S (S (S (S (S (S Z)))))
        (10 :: Nat) * (20 :: Nat) `shouldBe` (200 :: Nat)

    it "natSub" $ do
        Z           - Z           `shouldBe` Z
        S Z         - S (S Z)     `shouldBe` Z
        S (S (S Z)) - S (S Z)     `shouldBe` S Z
        (23 :: Nat) - (10 :: Nat) `shouldBe` (13 :: Nat)

    it "natEq" $ do
        Z           == Z           `shouldBe` True
        S Z         == S (S Z)     `shouldBe` False
        (23 :: Nat) == (11 :: Nat) `shouldBe` False
        (19 :: Nat) == (19 :: Nat) `shouldBe` True

    it "natCompare" $ do
        Z           <= Z           `shouldBe` True
        S Z         >  S (S Z)     `shouldBe` False
        (23 :: Nat) <  (11 :: Nat) `shouldBe` False
        (0 :: Nat)  <  (19 :: Nat) `shouldBe` True

    it "natEven" $ do
        isEven Z           `shouldBe` True
        isEven (S Z)       `shouldBe` False
        isEven (11 :: Nat) `shouldBe` False
        isEven (44 :: Nat) `shouldBe` True

    it "natDiv" $ do
        div (10 :: Nat) (4 :: Nat)  `shouldBe` (2 :: Nat)
        div (10 :: Nat) (40 :: Nat) `shouldBe` (0 :: Nat)
        div (11 :: Nat) (11 :: Nat) `shouldBe` (1 :: Nat)
        div (10 :: Nat) (1 :: Nat)  `shouldBe` (10 :: Nat)

    it "natMod" $ do
        mod (10 :: Nat) (4 :: Nat)  `shouldBe` (2 :: Nat)
        mod (10 :: Nat) (40 :: Nat) `shouldBe` (10 :: Nat)
        mod (11 :: Nat) (11 :: Nat) `shouldBe` (0 :: Nat)
        mod (10 :: Nat) (1 :: Nat)  `shouldBe` (0 :: Nat)

    it "treeIsEmpty" $ do
        isTreeEmpty Leaf `shouldBe` True
        isTreeEmpty (Node 3     Leaf                  Leaf)                   `shouldBe` False
        isTreeEmpty (Node "qwe" (Node "hi" Leaf Leaf) Leaf)                   `shouldBe` False
        isTreeEmpty (Node "Oh"  Leaf (Node "my" (Node "god" Leaf Leaf) Leaf)) `shouldBe` False

    it "treeCount" $ do
        countTreeElems Leaf                                                     `shouldBe` 0
        countTreeElems (Node 3 Leaf Leaf)                                       `shouldBe` 1
        countTreeElems (Node "qwe" (Node "hi" Leaf Leaf) Leaf)                  `shouldBe` 2
        countTreeElems (Node "Oh" Leaf (Node "my" (Node "god" Leaf Leaf) Leaf)) `shouldBe` 3
        countTreeElems (Node "Oh" (Node "god" Leaf Leaf) (Node "my" Leaf Leaf)) `shouldBe` 3

    it "treeContains" $ do
        containsTree Leaf 1 `shouldBe` False
        containsTree (Node 3 Leaf Leaf) 2                                `shouldBe` False
        containsTree (Node 5 (Node 4 Leaf Leaf) Leaf) 4                  `shouldBe` True
        containsTree (Node 6 Leaf (Node 4 (Node 0 Leaf Leaf) Leaf)) 3    `shouldBe` False
        containsTree (Node 10 (Node 5 Leaf Leaf) (Node 15 Leaf Leaf)) 15 `shouldBe` True

    it "treePut" $ do
        containsTree (putTree Leaf 1) 1                                               `shouldBe` True
        containsTree (putTree (Node 3 Leaf Leaf) 3) 3                                 `shouldBe` True
        containsTree (putTree (Node 5 (Node 2 Leaf Leaf) Leaf) 3) 3                   `shouldBe` True
        containsTree (putTree (Node 6 Leaf (Node 4 (Node 0 Leaf Leaf) Leaf)) 2) 2     `shouldBe` True
        containsTree (putTree (Node 10 (Node 5 Leaf Leaf) (Node 15 Leaf Leaf)) 20) 20 `shouldBe` True

    it "treeFromList" $ do
        containsTree (fromList [3..8])    3 `shouldBe` True
        containsTree (fromList [3..8])    4 `shouldBe` True
        containsTree (fromList [3..8])    5 `shouldBe` True
        containsTree (fromList [3..8])    6 `shouldBe` True
        containsTree (fromList [3..8])    7 `shouldBe` True
        containsTree (fromList [3..8])    8 `shouldBe` True
        containsTree (fromList [3..8])    9 `shouldBe` False
        containsTree (fromList [3..8])    1 `shouldBe` False
        containsTree (fromList [5, 2, 7]) 5 `shouldBe` True
        containsTree (fromList [5, 2, 7]) 2 `shouldBe` True
        containsTree (fromList [5, 2, 7]) 7 `shouldBe` True

