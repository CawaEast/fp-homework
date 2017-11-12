{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module LetParser where 

import           SExpr
import           AParser
import           Control.Arrow ((***))
import           Control.Monad(join)
import           Control.Applicative(Alternative(..))
import           Data.Monoid(Sum(..))
import           Data.List(find)
import           Data.Foldable(asum)
--import Data.Functor(Functor)

instance Applicative Parser => Monad Parser where
    return a = Parser (\text -> Just (a, text))
    
    -- Parser a -> (a -> Parser b) -> Parser b
    (>>=) (Parser f1) f2 = Parser (\text -> join $ fmap (uncurry runParser) $ fmap (f2 *** id) $ f1 text)

--type Ident = String

-- data Atom = N Integer | I Ident deriving Show
          
data Let = L Ident Expr deriving Show

data Expr = S [Atom] deriving Show
         
sumParser :: Parser [Atom]         
sumParser = spaces *> p2faParser atomParser <*> zeroOrMore (spaces *> satisfyC '+' *> spaces *> atomParser) <* spaces

letP :: Parser Char
letP = satisfyC 'l' *> satisfyC 'e' *> satisfyC 't'

letParser :: Parser Let
letParser = letP *> spaces1 *> (fmap L indent) <* spaces <* satisfyC '=' <*> (fmap S sumParser)

langLetParser :: Parser [Let]
langLetParser = oneOrMore (letParser <* zeroOrMore (satisfyC '\n'))

atomToInt :: [Let] -> Atom -> Maybe(Integer)
atomToInt _ (N a)   = Just ( a)
atomToInt l (I ident)  = fmap (\(L _ (S [N a])) -> ( a)) $ find (\(L i _) -> i == ident) l 

sumToInt :: [Let] -> Expr -> Maybe Integer
sumToInt l (S summ) = asum $ fmap (atomToInt l) summ

parseAndSimplify text = 
  where
    lets = runParser text langLetParser