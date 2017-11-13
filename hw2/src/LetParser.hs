{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module LetParser(Let(..), Expr, langLetParser, doSimpl) where 

import           AParser(Parser(..), satisfyC)
import           SExpr(oneOrMore, zeroOrMore, indent, spaces1, spaces, Atom(..), Ident, atomParser, p2faParser)
-- import           Control.Monad(join)
import           Data.List(find, intercalate, reverse)
import           Control.Applicative(Applicative(..))
--import Data.Functor(Functor)

-- instance Applicative Parser => Monad Parser where
    -- return a = Parser (\text -> Just (a, text))
    -- return a >>= f ≡ f a
    -- Parser (\text -> Just (a, text)) >>= f
    -- Parser $ join . fmap (uncurry runParser . first f) . (\text -> Just (a, text))
    -- Parser (\text -> join . fmap (uncurry runParser . first f) (Just (a, text)))
    -- Parser (\text -> join . fmap (uncurry runParser) (Just (f a, text)))
    -- Parser (\text -> join (Just ((uncurry runParser) (f a, text))))
    -- Parser (\text -> (uncurry runParser) (f a, text))
    -- Parser (\text ->  runParser (f a) text))
    -- Parser (runParser (f a)))
    -- f a
    
    -- Parser a -> (a -> Parser b) -> Parser b
    -- (>>=) (Parser f1) f2 = Parser $ join . fmap (uncurry runParser . first f2) . f1

-- type Ident = String

-- data Atom = N Integer | I Ident deriving Show
          
data Let = L Ident Expr deriving (Show, Eq)

newtype Expr = S [Atom] deriving (Show, Eq)
         
sumParser :: Parser [Atom]         
sumParser = spaces *> p2faParser atomParser <*> zeroOrMore (spaces *> satisfyC '+' *> spaces *> atomParser) <* spaces

letP :: Parser Char
letP = satisfyC 'l' *> satisfyC 'e' *> satisfyC 't'

letParser :: Parser Let
letParser = letP *> spaces1 *> fmap L indent <* spaces <* satisfyC '=' <*> fmap S sumParser

langLetParser :: Parser [Let]
langLetParser = oneOrMore (letParser <* zeroOrMore (satisfyC '\n'))

atomToInt :: [Let] -> Atom -> Maybe Integer
atomToInt _ (N a)   = Just a
atomToInt l (I ident)  = (\(L _ (S [N a])) -> a) <$> find (\(L i _) -> i == ident) l 

sumToInt :: [Let] -> Expr -> Maybe Integer
sumToInt l (S summ) = foldl1 (\a b -> fmap (+) a <*> b) (fmap (atomToInt l) summ)

validate :: Maybe (a, String) -> Maybe a
validate (Just (a, [])) = Just a
validate _              = Nothing

parseAndSimplify :: String -> Maybe [Let]
parseAndSimplify text = fmap reverse (validate (runParser langLetParser text) >>= flip simplify []) 
    
simplify :: [Let] -> [Let] -> Maybe [Let]
simplify [] inits = Just inits
simplify (L i e:xs) inits = fmap ((:inits) . L i . S . (:[]) . N) (sumToInt inits e) >>= simplify xs  

letsToString :: [Let] -> String
letsToString l = intercalate "\n" (map letToString l)

letToString :: Let -> String
letToString (L i e) = (\s -> 'l':'e':'t':' ':s) (i ++ " = " ++ exprToString e)

exprToString :: Expr -> String 
exprToString (S l) = intercalate "+" (map atomToString l) 

atomToString :: Atom -> String
atomToString (I i) = show i
atomToString (N i) = show i

doSimpl :: String -> String
doSimpl text = maybe "Nothing" letsToString (parseAndSimplify text)