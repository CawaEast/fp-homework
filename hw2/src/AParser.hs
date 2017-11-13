{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module AParser(Parser(..), abParser, abParser_, intPair, intOrUppercase, satisfy, posInt, satisfyC, first) where

import           Control.Applicative (Alternative (..))
import           Control.Arrow       ((***))
import           Control.Monad       (join)
import           Data.Char           (isDigit, isSpace, isUpper)
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
satisfyC c = satisfy (\ch -> c == ch)

first :: (a -> b) -> (a,c) -> (b,c)
first f1 = f1 *** id

instance Functor Parser where
  fmap f (Parser f_in) = Parser (fmap (first f) . f_in)
  
  -- fmap id (Parser f) ≡ id (Parser f)
  -- fmap id (Parser f) ≡
  -- (Parser (id f))    ≡
  -- (Parser f)         ≡
  -- id (Parser f)
  
instance Functor Parser => Applicative Parser where
  pure a = Parser (\t -> Just (a, t))  
 
  (Parser f1) <*> (Parser f2) = Parser (\str -> 
    let 
      res2 =((first *** f2) <$> f1 str)
    in
      fmap fst res2 <*> join (fmap snd res2))
      
  -- pure id <*> (Parser v) ≡ (Parser v)
  -- pure id <*> (Parser v) ≡ 
  -- Parser (\t -> Just (id, t)) <*> (Parser v) ≡ 
  -- Parser (\str -> let res2 =((first *** v) <$> (\t -> Just (id, t)) str) in fmap fst res2 <*> join (fmap snd res2)) ≡ 
  -- Parser (\str -> let res2 =((first *** v) <$> (Just (id, str)) in fmap fst res2 <*> join (fmap snd res2)) ≡ 
  -- Parser (\str -> let res2 =(Just (id, v str)) in fmap fst res2 <*> join (fmap snd res2)) ≡ 
  -- Parser (\str -> fmap fst (Just (id, v str)) <*> join (fmap snd (Just (id, v str)))) ≡ 
  -- Parser (\str -> (Just (id)) <*> join (Just (v str))) ≡ 
  -- Parser (\str -> (Just (id)) <*> v str) ≡ 
  -- Parser (\str -> v str) ≡ 
  -- Parser v  
      

      
aParser :: Parser Char      
aParser = satisfyC 'a'
bParser :: Parser Char
bParser = satisfyC 'b'
p2mpParser :: Parser a -> Parser (b -> (a, b))
p2mpParser = fmap (\a b -> (a, b))
clearParser :: Parser a -> Parser ()
clearParser = fmap (const ()) 
abParser :: Parser(Char, Char)
abParser = p2mpParser aParser <*> bParser
abParser_ :: Parser ()
abParser_ = clearParser abParser

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs
      
skipNonDigits :: Parser ()
skipNonDigits = clearParser (satisfy isSpace)

p2pspParser :: Parser a -> Parser (b -> (a -> [a]))
p2pspParser = fmap (\a _ b ->[a, b])

intPair :: Parser [Integer]
intPair = p2pspParser posInt <*> skipNonDigits <*> posInt


instance Applicative Parser => Alternative Parser where 
  empty = Parser (const Nothing)
  (Parser f1) <|> (Parser f2) =  Parser (\text -> f1 text <|> f2 text)
  
upperParser :: Parser Char  
upperParser = satisfy isUpper

intOrUppercase :: Parser ()
intOrUppercase = clearParser upperParser <|> clearParser posInt