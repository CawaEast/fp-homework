{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE FlexibleContexts    #-}
module Lib where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec(Parsec, ParseError, Token, runParser, notFollowedBy, try, between)
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.List(find)
import Data.Maybe(fromMaybe)
import Data.Either(fromRight)

data Expr  
    = Lit Integer
    | Var String
    | Let String Expr Expr 
    | Add Expr Expr  
    | Sub Expr Expr  
    | Mul Expr Expr  
    | Div Expr Expr  
    deriving (Show)
    
type Result a = Either MyError a

type VarType = (String, Integer)

data Variable = Variable Bool String Integer

--data Enviroment = Env (Result [VarType])
data Enviroment = Env (State String (Result Integer))

putVal ::  Enviroment -> VarType -> Enviroment
putVal (Env l) (a, b) = Env (state func)
  where
    --func :: String -> (State String (Result Integer))
    func = (\name -> if name == a then (return b, a) else runState l a)
    
putResVal :: Enviroment -> (String, Result Integer) -> Enviroment
putResVal e (a, b) = func
  where
    func = either errorEnv ((putVal e) . (\q -> (a, q))) b
    
initEnv :: Enviroment
initEnv = Env (state (\str -> (throwError VarNotFound, str))) 

errorEnv :: MyError -> Enviroment
errorEnv e = (Env $ state (\q ->(throwError e, q)))

putMut :: Enviroment -> VarType -> Enviroment 
putMut (Env l) (a, b) = Env (state (\name -> 
    if (fst $ runState l a) == throwError VarNotFound
    then (return b, a)
    else ((fst $ runState l a) >>= (\n -> (throwError VarHasBeenSet)), name)))

putReMut :: Enviroment -> VarType -> Enviroment 
putReMut (Env l) (a, b) = Env $ state (\name -> (((fst $ runState l name) >>= (\res ->  if name == a 
                                                                                        then return b
                                                                                        else return res)), name))
    
data MyError    
    = SomeInitianError
    | VarNotFound
    | VarHasBeenSet
    | DivByZero
    | ParseError
    deriving (Show, Eq, Ord)

--instance Except MyError where
    --noMsg = SomeInitianError
    --strMsg str = SomeInitianError
    
expr2R :: Expr -> Reader Enviroment (Result Integer)
expr2R (Lit i) = reader (\_ -> return i)
expr2R (Add a b) = liftA2 (+) <$> (expr2R a) <*> (expr2R b) 
expr2R (Sub a b) = liftA2 (-) <$> (expr2R a) <*> (expr2R b) 
expr2R (Mul a b) = liftA2 (*) <$> (expr2R a) <*> (expr2R b) 
expr2R (Div a b) = return (\a1 b1 -> 
                            if b1 == (Right 0) 
                            then throwError DivByZero 
                            else div <$> a1 <*> b1) <*> (expr2R a) <*> (expr2R b) 
expr2R (Var n) = reader (\(Env l) -> fst $ runState l n)
expr2R (Let name val expr) = withReader (\e -> putResVal e (name, (runReader (expr2R val) e))) (expr2R expr)
--expr2R (Mut name val expr) = withReader (\e -> putResVal e (name, (runReader (expr2R val) e))) (expr2R expr)

type Parser = Parsec MyError String

sc :: Parser ()
sc = L.space Text.Megaparsec.Char.space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

integer :: Parser Integer
integer = lexeme L.decimal

rws :: [String] -- list of reserved words
rws = ["let", "mut", "in"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

            
exprParser :: Parser Expr
exprParser = makeExprParser term operators
            
term :: Parser Expr
term    = atomParser
        <|> parens letParser
        <|> parens exprParser

atomParser :: Parser Expr
atomParser  =   (Lit <$> integer) 
            <|> (Var <$> identifier)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


letParser :: Parser Expr        
--letParser = parens (Let <$ rword "let" <*> identifier <* symbol "=" <*> exprParser <* rword "in" <*> exprParser) 
letParser = do
    rword "let"
    i <- identifier
    _ <- symbol "="
    a <- exprParser
    rword "in"
    b <- exprParser
    return (Let i a b)
        
operators :: [[Operator Parser Expr]]
operators =[
    [InfixL (Mul <$ symbol "*"),  InfixL (Div <$ symbol "/")],
    [InfixL (Add <$ symbol "+"),  InfixL (Sub <$ symbol "-")]]

unifyA :: (Either (ParseError (Token String) MyError) Expr) -> Result Expr
unifyA = either (\_ -> throwError ParseError) return 

simpleRunA :: Parser Expr -> String -> Result Expr
simpleRunA p i = unifyA $ runParser p "" i