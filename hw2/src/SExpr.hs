{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SExpr where 

import           AParser
import           Control.Arrow ((***))
import           Data.Char(isSpace, isAlpha, isAlphaNum)
import           Control.Applicative(Alternative(..))
--import Data.Functor(Functor)

closure :: (String -> (Maybe (a, String))) -> (String -> (Maybe ([a], String)))
closure f = (\text ->
  let
    res = f text
  in (res >>= (\(a, text1) -> fmap ((a:) *** id) $ closure f text1)) 
     <|> fmap ((:[]) *** id) res)


oneOrMore :: Parser a -> Parser [a]
oneOrMore (Parser f) = Parser $ closure f

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> Parser (\text -> Just ([], text))

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

p2faParser :: Parser a -> Parser ([a] -> [a])
p2faParser (Parser f) = Parser (\text -> fmap ((\a -> (a:)) *** id) (f text))

indent :: Parser String
indent = p2faParser (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)


type Ident = String

data Atom = N Integer 
          | I Ident deriving Show

data SExpr = A Atom
           | Comb [SExpr] deriving Show
           
spaces1 :: Parser String
spaces1 = oneOrMore $ satisfy isSpace           

atomParser :: Parser Atom
atomParser = fmap I indent <|> fmap N posInt

combParser :: Parser [SExpr]
combParser = (satisfyC '(') *> oneOrMore sexprParser <* (satisfyC ')')

sexprParser :: Parser SExpr
sexprParser = spaces *> (fmap A atomParser <|> fmap Comb combParser) <* spaces