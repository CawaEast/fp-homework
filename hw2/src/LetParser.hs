{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module LetParser where 

import           SExpr
import           AParser
import           Control.Arrow ((***))
import           Control.Monad(join)
import           Control.Applicative(Alternative(..))
import           Data.Monoid(Sum(..))
import           Data.List(find, intercalate, reverse)
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
sumToInt l (S summ) = foldl1 (\a b -> (fmap (+) a) <*> b) (fmap (atomToInt l) summ)

validate :: Maybe (a, String) -> Maybe a
validate (Just (a, [])) = Just a
validate _              = Nothing

parseAndSimplify :: String -> Maybe [Let]
parseAndSimplify text = fmap reverse ((validate $ runParser langLetParser text) >>= (flip simplify) []) 
    
simplify :: [Let] -> [Let] -> Maybe [Let]
simplify [] inits = Just inits
simplify ((L i e):xs) inits = (fmap (:inits) (fmap ((L i) . S . (:[]) . N) (sumToInt inits e))) >>= simplify xs  

letsToString :: [Let] -> String
letsToString l = intercalate "\n" (map letToString l)

letToString :: Let -> String
letToString (L i e) = (\s -> 'l':'e':'t':' ':s) (i ++ " = " ++ (exprToString e))

exprToString :: Expr -> String 
exprToString (S l) = intercalate "+" (map atomToString l) 

atomToString :: Atom -> String
atomToString (I i) = (show i)
atomToString (N i) = (show i)

doSimpl :: String -> String
doSimpl text = maybe "Nothing" letsToString (parseAndSimplify text)