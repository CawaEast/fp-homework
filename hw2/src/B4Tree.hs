{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B4Tree(Tree(..), Functor(..), Applicative(..), Foldable(..), Traversable(..)) where

-- import Prelude((.))
import Data.Monoid(Monoid(..))

data Tree a = Leaf | Node a (Tree a) (Tree a)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
  
  
  -- fmap id a      ≡ id a 

  -- fmap id Leaf   ≡ id Leaf 
  -- fmap id Leaf   ≡  
  -- Leaf           ≡
  -- id Leaf

  -- fmap id (Node a l r)                   ≡ id (Node a l r)
  -- fmap id (Node a l r)                   ≡ 
  -- Node (id a) (fmap id l) (fmap id r)    ≡
  -- Node a l r                             ≡
  -- id (Node a l r)     
                         
    
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

        
instance Applicative Tree where
  pure x = Node x Leaf Leaf
  Leaf <*> _ = Leaf
  _ <*> Leaf = Leaf
  (Node lv ll lr) <*> (Node rv rl rr) = Node (lv rv) (ll <*> rl) (lr <*> rr)

  
class Foldable t where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

  
class (Functor t, Foldable t) => Traversable t where
    traverse    :: Applicative f => (a -> f b) -> t a -> f (t b)
    --sequenceA   :: Applicative f => t (f a) -> f (t a)
    
instance Traversable Tree where
  traverse _ Leaf         = pure Leaf
  traverse f (Node k l r) = (fmap Node (f k)) <*> traverse f l <*> traverse f r
   