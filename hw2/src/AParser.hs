{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AParser(Parser(..), abParser, abParser_, intPair, intOrUppercase, satisfy, posInt, satisfyC) where 

import           Control.Arrow ((***))
import           Control.Monad(join)
import           Data.Char(isDigit, isUpper, isSpace)
import           Control.Applicative(Alternative(..))
--import Data.Functor(Functor)


newtype Parser a = Parser { runParser :: String -> Maybe (a, String)}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing              -- fail on the empty input
    f (x:xs)                    -- check if x satisfies the predicate
                                -- if so, return x along with the remainder
                                -- of the input (that is, xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing     -- otherwise, fail
      
satisfyC :: Char -> Parser Char
satisfyC = (\c -> satisfy (\ch -> c == ch))
     
instance Functor Parser where
  fmap f (Parser f_in) = Parser (\str -> fmap (first f) $ f_in str)
    where
      first :: (a -> b) -> (a,c) -> (b,c)
      first f1 = (f1 *** id)

instance Functor Parser => Applicative Parser where
  pure a = Parser (\_ -> Just (a, []))
  
  (Parser f1) <*> (Parser f2) = Parser (\str -> 
    let 
      res2 = fmap ((\f -> (f *** id)) *** f2) $ f1 str
    in
      (fmap fst res2) <*> (join $ fmap snd res2))
      
p2mpParser :: Parser a -> Parser (b -> (a, b))
p2mpParser = fmap (\a -> (\b -> (a, b)))
clearParser :: Parser a -> Parser ()
clearParser p = fmap (\_ -> ()) p
abParser :: Parser(Char, Char)
abParser = p2mpParser (satisfyC 'a') <*> (satisfyC 'b')
abParser_ :: Parser ()
abParser_ = clearParser abParser

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

p2pspParser :: (Parser a) -> Parser (a -> [a])
p2pspParser = fmap (\a ->(\b ->[a, b]))

intPair :: Parser [Integer]
intPair = p2pspParser posInt <* (satisfy isSpace) <*> posInt


instance Applicative Parser => Alternative Parser where 
  empty = Parser (\_ -> Nothing)
  (Parser f1) <|> (Parser f2) =  Parser (\text -> (f1 text) <|> (f2 text))

upperParser :: Parser Char
upperParser = satisfy isUpper

intOrUppercase :: Parser ()
intOrUppercase = clearParser upperParser <|> clearParser posInt