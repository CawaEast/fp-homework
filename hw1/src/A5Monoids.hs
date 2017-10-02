module A5Monoids where

import           Data.Either    (either)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup (..))
import           TreePrinters   (Tree (..))

maybeConcat :: [Maybe [t]]-> [t]
maybeConcat = foldMap (fromMaybe [])

eitherConcat ::(Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldMap (either (\a -> (a, mempty)) (\a -> (mempty, a)))

data NonEmpty a = a :| [a] deriving (Eq, Ord, Show, Read)

instance Semigroup (NonEmpty a) where
    (<>) (z :| zs) (x :| xs) = z :| (zs ++ [x] ++ xs)

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity(a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mappend  = (<>)
    mempty = Identity mempty

newtype Name = Name String

instance Semigroup Name where
    (<>) (Name a) (Name b) =  Name (a ++ "." ++ b)

instance Monoid Name where
    mappend (Name "") a = a
    mappend a (Name "") = a
    mappend a b         = (<>) a b
    mempty = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup a => Semigroup (Endo a) where
    (<>) (Endo f1) (Endo f2) =  Endo (\c -> f1 c <> f2 c)

instance (Semigroup a, Monoid a) => Monoid (Endo a) where
    mappend = (<>)
    mempty = Endo (const mempty)

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    (<>) (Arrow f1) (Arrow f2) =  Arrow (\c -> f1 c <> f2 c)

instance (Semigroup b, Monoid b) => Monoid    (Arrow a b) where
    mappend = (<>)
    mempty = Arrow (const mempty)

--data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

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
