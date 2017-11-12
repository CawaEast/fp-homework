module SimpleSpec where

import           B1Math
import           B2NonDetMath

import           Test.Hspec

spec :: Spec
spec = do
  it "Expr" $ do
    eval (Const 42)                   `shouldBe` Right 42
    eval (Const (-42))                `shouldBe` Right (-42)
    eval (add1 (Const 1) (Const 2))    `shouldBe` Right 3
    eval (add1 (Const 4) (Const 2))    `shouldBe` Right 6
    eval (mul1 (Const 6) (Const 7))    `shouldBe` Right 42
    eval (mul1 (Const (-1)) (Const 8)) `shouldBe` Right (-8)
    eval (sub1 (Const 1) (Const 2))    `shouldBe` Right (-1)
    eval (div1 (Const 5) (Const 2))    `shouldBe` Right 2
    eval (div1 (Const 5) (Const (-2))) `shouldBe` Right (-3)
    eval (div1 (Const 5) (Const 0))    `shouldBe` Left DivByZero
    eval (pow1 (Const 3) (Const 3))    `shouldBe` Right 27
    eval (pow1 (Const 9) (Const (-9))) `shouldBe` Left NegExp
  
  it "bin" $ do
    bin 1 `shouldBe` [[0], [1]]
    bin 2 `shouldBe` [[0, 0], [1, 0], [0, 1], [1, 1]]
    bin 3 `shouldBe` [[0, 0, 0], [1, 0, 0], [0, 1, 0], [1, 1, 0], [0, 0, 1], [1, 0, 1], [0, 1, 1], [1, 1, 1]]
  
  it "comb" $ do
    combinations 4 1 `shouldBe` [[1], [2], [3], [4]]
    combinations 4 2 `shouldBe` [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]
    combinations 4 3 `shouldBe` [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
    combinations 4 4 `shouldBe` [[1, 2, 3, 4]]
    
  it "perm" $ do
    permutations [1, 2, 3]    `shouldBe` [[1,2,3],[2,3,1],[3,1,2],[1,3,2],[3,2,1],[2,1,3]]
    permutations [1, 2]       `shouldBe` [[1, 2], [2, 1]]
    permutations [1]          `shouldBe` [[1]]
    permutations [1, 2, 3, 4] `shouldBe`  [ [1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3],
                                            [1,3,4,2],[3,4,2,1],[4,2,1,3],[2,1,3,4],
                                            [1,4,2,3],[4,2,3,1],[2,3,1,4],[3,1,4,2],
                                            [1,2,4,3],[2,4,3,1],[4,3,1,2],[3,1,2,4],
                                            [1,4,3,2],[4,3,2,1],[3,2,1,4],[2,1,4,3],
                                            [1,3,2,4],[3,2,4,1],[2,4,1,3],[4,1,3,2]]