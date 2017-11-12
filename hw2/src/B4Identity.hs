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
    
    {-# LAWS
            1. pure id <*> v ≡ v
            2. pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
            3. pure f <*> pure x ≡ pure (f x)
            4. u <*> pure y ≡ pure ($ y) <*> u
    #-}
        
instance Applicative Identity where
    pure    = Identity
    (<*>) f a = pure ((runIdentity f) (runIdentity a))

    -- 1. (<*>) (pure id) v                                     ≡ v
    -- pure ((runIdentity (pure id)) (runIdentity v))           ≡ 
    -- Identity ((runIdentity (Identity id)) (runIdentity v))   ≡ 
    -- Identity (id (runIdentity v))                            ≡ 
    -- Identity (runIdentity v)                                 ≡ 
    -- v                                                         

    -- 2. (<*>)((<*>)((<*>) (pure (.)) u) v) w ≡ (<*>) u (<*> v w)  
    -- pure ((runIdentity (pure ((runIdentity ((<*>) (pure (.)) u)) (runIdentity v)))) (runIdentity w))
    -- pure ((runIdentity (pure ((runIdentity ((runIdentity (pure (.))) (runIdentity u))) (runIdentity v)))) (runIdentity w))
    -- pure ((runIdentity (pure ((runIdentity ((.) (runIdentity u))) (runIdentity v)))) (runIdentity w))
    -- pure (((runIdentity ((.) (runIdentity u)))(runIdentity v))(runIdentity w))
    -- pure ((runIdentity u) ((runIdentity v)(runIdentity w)))
    
    -- pure ((runIdentity u) (runIdentity (pure ((runIdentity v) (runIdentity w)))))
    -- pure ((runIdentity u) ((runIdentity v) (runIdentity w)))
    
    -- 3.
    
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable Identity where
    foldr f z i = f (runIdentity i) z
    foldMap f   = f . runIdentity

class (Functor t, Foldable t) => Traversable t where
    --traverse    :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA   :: Applicative f => t (f a) -> f (t a)
    
instance Traversable Identity where
    --traverse f a = sequenceA id
    sequenceA a  = pure pure <*> runIdentity a  
