module ParserSpec where

import           AParser
import           SExpr
import           LetParser
import           Data.Char(isUpper)

import           Test.Hspec

spec :: Spec
spec = do
  it "parserAB" $ do
    runParser abParser  "abcde"        `shouldBe` Just (('a', 'b'), "cde")
    runParser abParser  "bad"          `shouldBe` Nothing
    runParser abParser  "aesds"        `shouldBe` Nothing
    
  it "parserAB_" $ do  
    runParser abParser_ "abcde"        `shouldBe` Just ((), "cde")
    runParser abParser_ "bad"          `shouldBe` Nothing
    runParser abParser_ "BBBBB"        `shouldBe` Nothing
    
  it "parserIntPair" $ do 
    runParser intPair   "12 42"        `shouldBe` Just ([12, 42], "")
    runParser intPair   "12 42 32"     `shouldBe` Just ([12, 42], " 32")
    runParser intPair   "l12 42fd"     `shouldBe` Nothing
    runParser intPair   "s 12 42 "     `shouldBe` Nothing
    
  it "parserIoU" $ do
    runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
    runParser intOrUppercase "XYZ"     `shouldBe` Just ((), "YZ")
    runParser intOrUppercase "foo"     `shouldBe` Nothing
    
  it "parserFunctor" $ do
    runParser (fmap fst abParser)  "abcde"  `shouldBe` Just ('a', "cde")
    runParser (fmap snd abParser)  "abcde"  `shouldBe` Just ('b', "cde")
    runParser (fmap snd abParser)  "rbcde"  `shouldBe` Nothing
    runParser (fmap head intPair)  "12 42"   `shouldBe` Just (12, "")
    runParser (fmap last intPair)  "12 42"   `shouldBe` Just (42, "")
    runParser (fmap head intPair)  "rbcde"   `shouldBe` Nothing
    
  it "parser01alot" $ do
    runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
    runParser ( oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
    runParser (zeroOrMore (satisfy isUpper)) "aBCdEfgH" `shouldBe` Just ("", "aBCdEfgH")
    runParser ( oneOrMore (satisfy isUpper)) "aBCdEfgH" `shouldBe` Nothing
    
    
  it "parserSpaces" $ do
    runParser spaces "     35435"   `shouldBe` Just ("     ", "35435")
    runParser spaces "35435"        `shouldBe` Just ("", "35435")
    runParser spaces "     354  35" `shouldBe` Just ("     ", "354  35")
    
  it "parseIndent" $ do
    runParser indent  "foobar baz"  `shouldBe` Just ( "foobar", " baz")
    runParser indent  "foo33fA"     `shouldBe` Just ("foo33fA", "")
    runParser indent  "2bad"        `shouldBe` Nothing
    runParser indent  ""            `shouldBe` Nothing
    
  it "SExprTest" $ do
    runParser sexprParser "5"                   `shouldBe` Just (A (N 5),"")
    runParser sexprParser "foo3"                `shouldBe` Just (A (I "foo3"),"")
    runParser sexprParser "(bar (foo) 3 5 874)" `shouldBe` Just (Comb [A (I "bar"), (Comb [A (I "foo")]), A (N 3), A (N 5), A (N 874)],"")
    runParser sexprParser "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe`  Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")
    runParser sexprParser "( lots of ( spaces in ) this ( one ))" `shouldBe`  Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")
    
  it "letParser" $ do
    doSimpl "let x = 1 + 2 + 5\nlet   y = x+x \n let z=0+    x   + y + 8"       `shouldBe` "let x = 8\nlet y = 16\nlet z = 32"
    doSimpl "let x = 1 + 2 + 5\nlet   y = x+x \n let z=0+    x   + y33 + 8443"  `shouldBe` "Nothing"
   