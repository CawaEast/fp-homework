{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B4Identity(Identity(..), Functor(..), Applicative(..), Foldable(..), Traversable(..))where

import Prelude ((.))
import Data.Monoid(Monoid)

newtype Identity a = Identity { runIdentity :: a }

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Identity where
    fmap f = Identity . f . runIdentity
    
    -- 1. fmap id                   ≡ id
    -- Identity . id . runIdentity  ≡  
    -- Identity . runIdentity       ≡  
    -- id                           ≡     

    -- 2. fmap f . fmap g ≡ fmap (f . g)
    -- (Identity . f . runIdentity) . (Identity . g . runIdentity)  ≡
    -- Identity . f . runIdentity . Identity . g . runIdentity      ≡
    -- Identity . f . id . g . runIdentity                          ≡
    -- Identity . f . g . runIdentity                               ≡
    -- fmap(f . g)                                                  ≡

    -- fmap . runIdentity f ≡ Identity . (runIdentity f) . runIdentity
    
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

        
instance Applicative Identity where
    pure    = Identity
    (<*>)   = fmap . runIdentity

    -- 1. (<*>) (pure id) v                     ≡ v
    -- ((fmap . runIdentity) (Identity id) v)   ≡ 
    -- fmap id v                                ≡  
    -- v                                                         

    -- 2. pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w) 
    
    -- pure (.) <*> u <*> v <*> w               ≡ 
    -- (Identity (.)) <*> u <*> v <*> w         ≡ 
    -- (fmap (.)  u) <*> v <*> w                ≡
    -- fmap (fmap (.) u) v) <*> w               ≡
    -- Identity (u . v) <*> w                   ≡ 
    -- fmap (u . v) w                           ≡
    -- Identity ((u . v) runIdentity w)
    
    -- u <*> (v <*> w) = 
    -- u <*> (fmap (runIdentity v) w)               ≡ 
    -- fmap (runIdentity u) (fmap (runIdentity v) w)≡
    -- Identity (u (v runIdentity w))               ≡
    -- Identity ((u . v) runIdentity w)
    
    -- 3. pure f <*> pure x ≡ pure (f x) 
    -- Identity f <*> Identity x    ≡ 
    -- fmap f (Identity x)          ≡ 
    -- Identity (f x)               ≡ 
    -- pure (f x)
    
    -- 4. u <*> pure y ≡ pure ($ y) <*> u
    
    -- u <*> pure y                         ≡ 
    -- fmap (runIdentity u) (Identity y)    ≡
    -- Identity ((runIdentity u) y)
    
    -- pure ($ y) <*> u                     ≡
    -- fmap (runIdentity Identity ($ y)) u  ≡
    -- fmap ($ y) u                         ≡
    -- Identity ($ y) (runIdentity u)       ≡ 
    -- Identity ((runIdentity u) y)
    
    
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldMap :: Monoid m => (a -> m) -> t a -> m
    fold :: Monoid m => (t m) -> m

instance Foldable Identity where
    foldr f z i = f (runIdentity i) z
    foldMap f   = f . runIdentity
    fold        = runIdentity
    
    -- foldMap id ≡ fold
    -- foldMap id       ≡ 
    -- id . runIndetity ≡
    -- runIdentity      ≡ 
    -- fold
    
    -- foldMap f ≡ fold . fmap f
    
    -- foldMap f        ≡
    -- f . runIdentity
    
    -- fold . fmap f                            ≡ 
    -- fold (Identity . f . runIdentity)        ≡
    -- runIdentity . Identity . f . runIdentity ≡
    -- f . runIdentity

class (Functor t, Foldable t) => Traversable t where
    --traverse    :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA   :: Applicative f => t (f a) -> f (t a)
    
instance Traversable Identity where
    -- (runIdentity arg) = (t b) => pure pure <*> (t b) = Identity (t b)
    --traverse f a = sequenceA id
    sequenceA a  = pure pure <*> runIdentity a
    
    -- 1. (t . sequenceA) arg ≡ (sequenceA . fmap t) arg
    
    -- (t . sequenceA) arg                  ≡ 
    -- t (pure pure <*> (runIdentity arg))  ≡
    -- t (Identity (t b))
    
    -- (sequenceA . fmap t) arg             ≡ 
    -- sequenceA (fmap t arg)               ≡
    -- sequenceA (Identity (t (t b)))       ≡
    -- t (Identity (t b))

    --  arg = t a => fmap Identity arg = t (Identity a)
    -- 2. (sequenceA . fmap Identity) arg ≡ Identity arg
    -- (sequenceA . fmap Identity) arg  ≡
    -- sequenceA (fmap Identity arg)    ≡
    -- Identity (t a)                   ≡
    -- Identity arg

    -- arg = Identity (t1 (t2 a)) => fmap Compose arg = Identity (Compose t1 t2 a)
    -- arg = Identity (t1 (t2 a)) => sequenceA arg = t1 (Identity (t2 a))
    
    -- 3. sequenceA . fmap Compose ≡ Compose . fmap sequenceA . sequenceA 
    -- sequenceA . fmap Compose arg ≡ 
    -- sequenceA (fmap Compose arg) ≡
    -- Compose t1 t2 (Identity a)
      
    -- (Compose . fmap sequenceA . sequenceA) arg
    -- fmap sequenceA (sequenceA arg) ≡ 
    -- fmap sequenceA  (t1 (Identity (t2 a))) ≡ 
    -- (t1 (t2 (Identity a))) => 
    -- Compose (t1 (t2 (Identity a))) ≡ 
    -- Compose t1 t2 (Identity a)  
