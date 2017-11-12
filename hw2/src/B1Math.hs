{-# LANGUAGE TypeOperators #-}

module B1Math where

import Control.Monad ((<=<))
import Data.Maybe (fromMaybe, isJust)

import qualified Control.Category as C

data Expr = Const Int 
--  | Common ([Int] -> (Either String Int)) [Expr] [(Either String Int)]
  | Unary (Int -> (Either Error Int)) Expr 
  | Binary (Int -> Int -> (Either Error Int)) Expr Expr
--  | BinaryMon (Int -> Either String (Int -> (Either String Int))) Expr Expr
  
data Error = DivByZero | NegExp deriving (Show, Eq) 
  
eval :: Expr -> (Either Error Int)
--calc (Common f a) = (map eval a) >>= f
--calc (Unary f a) = Common (\e -> f (head e)) [a]
--calc (Binary f a b) = Common (\e -> f (head e) (f head $ tail e)) [a, b]
eval (Unary f a) = (eval a) >>= f
eval (Binary f a b) = either Left (\half -> eval $ Unary (f half) b) $ (eval a)
eval (Const a) = Right a

add1 :: Expr -> Expr -> Expr
add1 = Binary (\a b -> Right (a + b)) 

sub1 :: Expr -> Expr -> Expr
sub1 = Binary (\a b -> Right (a - b)) 

mul1 :: Expr -> Expr -> Expr
mul1 = Binary (\a b -> Right (a * b)) 

div1 :: Expr -> Expr -> Expr
div1 = Binary (\a b -> if b == 0
                       then Left DivByZero
                       else Right (a `div` b))
                             
pow1 :: Expr -> Expr -> Expr
pow1 = Binary (\a b -> if b < 0
                       then Left NegExp
                       else Right (a ^ b)) 

      
data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b

partial     :: (a -> Maybe b) -> a ~> b
partial = Partial

total       :: (a -> b) -> a ~> b
total f = Partial $ Just . f

apply       :: (a ~> b) -> a -> Maybe b
apply (Partial f) a = f a
apply (Defaulted f b) a = Just $ fromMaybe b $ apply f a
-- apply $ Partial $ apply f  ≡ apply f => f ≡ Partial $ apply f

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f a b = fromMaybe b $ apply f a

withDefault :: (a ~> b) -> b -> (a ~> b)  -- Add a default value to a partial function. If the function was already
withDefault = Defaulted                   -- defaulted, override the value with the new default.


isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f a = isJust $ apply f a

orElse      :: (a ~> b) -> (a ~> b) -> a ~> b  -- Create a new partial function where the domain is the combination
orElse f g = Partial (\a -> maybe (apply g a) Just (apply f a))  
    
  
instance C.Category (~>) where

  id = Partial return
  -- id . f = f . id = f  
  -- id . f = f
  -- Partial $ apply (Partial return) <=< apply f ≡    (Rule : apply (Partial f) ≡ f)
  -- Partial $ return <=< apply f ≡                    (Rule: return <=< g ≡ g)
  -- Partial $ apply f ≡                               (Rule : apply (Partial f) ≡ f)
  -- f
  
  -- f . id = f
  -- Partial $ apply f <=< apply (Partial return) ≡    (Rule : apply (Partial f) ≡ f)
  -- Partial $ apply f <=< return ≡                    (Rule: g <=< return ≡ g)
  -- Partial $ apply f ≡                               (Rule : apply (Partial f) ≡ f)
  -- f
  
  -- (.):: b~>c -> a~>b -> a~>c
  (.) f g = Partial $ apply f <=< apply g 
  -- f . (g . h) ≡ (f . g) . h
  -- Partial $ apply f <=< apply (Partial $ apply g <=< apply h) ≡    
  -- Partial $ apply (Partial $ apply f <=< apply g) <=< apply h =>
  --                                                    (Rule : apply (Partial f) ≡ f)
  -- Partial $ (apply f <=< apply g) <=< apply h ≡    
  -- Partial $ apply f <=< (apply g <=< apply h)
  --                                                    (Rule : (f <=< g) <=< h ≡ f <=< (g <=< h))