{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B4Either(Either(..), Functor(..), Applicative(..), Foldable(..), Traversable(..)) where

import Data.Monoid(Monoid, mempty)

data Either a b = Left a | Right b

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor (Either a) where
  fmap _ (Left  l) = Left l
  fmap f (Right b) = Right (f b)
    
    -- 1. fmap id a                 ≡ id a
    -- fmap id (Left l)             ≡  
    -- Left l                       ≡  
    -- id (Left l)                        
    -- fmap id (Right b)            ≡
    -- Right $ id b                 ≡  
    -- Right b                           
    
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

        
instance Applicative (Either a) where
  pure = Right
  (Right f) <*> (Right m) = Right (f m)
  (Left l)  <*> _         = Left l
  _         <*> (Left m)  = Left m

  
class Foldable t where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable (Either a) where
  foldMap f (Right a) = f a
  foldMap _ (Left  _) = mempty

  
class (Functor t, Foldable t) => Traversable t where
  traverse    :: Applicative f => (a -> f b) -> t a -> f (t b)
  --sequenceA   :: Applicative f => t (f a) -> f (t a)
    
instance Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right y) = fmap Right (f y)
   