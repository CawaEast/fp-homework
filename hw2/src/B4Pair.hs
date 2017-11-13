{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B4Pair(Pair(..), Functor(..), Applicative(..), Foldable(..), Traversable(..))where

import Data.Monoid(Monoid(..))

data Pair a b = Pair {fst :: a, snd :: b}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
  
  -- fmap id (Pair a b) ≡ (Pair a b)  
  -- fmap id (Pair a b) ≡   
  -- (Pair a (id b)) ≡   
  -- (Pair a b)   
       
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

        
instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  (Pair a f) <*> (Pair b x) = Pair (a `mappend` b) (f x)

  
class Foldable t where
  --foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m
  
instance Foldable (Pair a) where
  foldMap f (Pair _ x) = f x
  
    
class (Functor t, Foldable t) => Traversable t where
    traverse    :: Applicative f => (a -> f b) -> t a -> f (t b)
    --sequenceA   :: Applicative f => t (f a) -> f (t a)
    
instance Traversable (Pair a) where
  traverse f (Pair x y) = fmap (Pair x) (f y)
   