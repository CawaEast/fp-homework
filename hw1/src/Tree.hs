{-# LANGUAGE DeriveFunctor #-}

module Tree where

import           Data.Semigroup (Semigroup (..))

-- There is warning, if declaration of type and some imolementations are in
-- different places, so I have to make 1 file to rule them all.

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Functor, Show)

--4th task, the first
instance Foldable Tree where
    foldMap _ Leaf         = mempty
    foldMap f (Node k l r) = foldMap f l `mappend` f k `mappend` foldMap f r
    foldr _ z Leaf         = z
    foldr f z (Node k l r) = foldr f (f k (foldr f z r)) l

--5th task last part
instance (Ord a) => Semigroup (Tree a) where
    (<>) Leaf a = a
    (<>) a Leaf = a
    (<>) (Node a1 b1 c1) (Node a2 b2 c2)
      | a1 == a2    = Node a1 (b1 <> b2) (c1 <> c2)
      | a1 < a2     = Node a1 b1 (c1 <> Node a2 Leaf c2) <> b2
      | otherwise   = Node a1 (b1 <> Node a2 b2 Leaf) c1 <> c2


instance (Ord a) => Monoid (Tree a) where
    mappend = (<>)
    mempty = Leaf

