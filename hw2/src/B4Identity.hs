{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B4Identity where

import Prelude (id, (.))
import Data.Monoid(Monoid)

newtype Identity a = Identity { runIdentity :: a }

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    {-# LAWS
        1. fmap id         ≡ id
        2. fmap f . fmap g ≡ fmap (f . g)
    #-}

instance Functor Identity where
    fmap f = Identity . f . runIdentity
    
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a
    
    
instance Applicative Identity where
    pure    = Identity
    (<*>)   = fmap . runIdentity
    (*>) _ b = b
    (<*) a _ = a

class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable Identity where
    foldr f z i = f (runIdentity i) z
    foldMap f   = f . runIdentity

class (Functor t, Foldable t) => Traversable t where
    traverse    :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA   :: Applicative f => t (f a) -> f (t a)
    
instance Traversable Identity where
    traverse f a = sequenceA id
    sequenceA a  = pure pure <*> runIdentity a  
