{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module AParser(Parser(..), abParser, abParser_, intPair, intOrUppercase, satisfy, posInt, satisfyC, first) where

import           Control.Applicative (Alternative (..))
import           Control.Arrow       ((***))
import           Control.Monad       (join, void)
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

  -- fmap id (Parser f) 