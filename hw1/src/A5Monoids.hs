module A5Monoids where

import           Data.Either    (either)
import           Data.Maybe     (fromMaybe)
import           Data.Semigroup (Semigroup (..))

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
    mempty   = Identity mempty

newtype Name = Name String deriving (Eq)

instance Semigroup Name where
    (<>) (Name a) (Name b) =  Name (a ++ "." ++ b)

instance Monoid Name where
    mappend (Name "") a         = a
    mappend a         (Name "") = a
    mappend a         b         = (<>) a b
    mempty                      = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup a => Semigroup (Endo a) where
    (<>) (Endo f1) (Endo f2) =  Endo (f2 . f1)

instance (Semigroup a, Monoid a) => Monoid (Endo a) where
    mappend = (<>)
    mempty  = Endo id

newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    (<>) (Arrow f1) (Arrow f2) =  Arrow (\c -> f1 c <> f2 c)

instance (Semigroup b, Monoid b) => Monoid (Arrow a b) where
    mappend = (<>)
    mempty  = Arrow (const mempty)

--implemented in Tree.hs
--instance (Ord a) => Semigroup (Tree a) where
--instance (Ord a) => Monoid (Tree a) where
