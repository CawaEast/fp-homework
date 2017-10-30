{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B3Monad where

import Prelude (id)
--import B3MonadFish(MonadFish)
--import B3MonadJoin(MonadJoin)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    {-# LAWS
        1. fmap id         ≡ id
        2. fmap f . fmap g ≡ fmap (f . g)
    #-}

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {-# LAWS
        1. m >>= return    ≡ m
        2. return a >>= f  ≡ f a
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    #-}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    {-# LAWS
        1. f >=> returnFish ≡ f
        2. returnFish >=> f ≡ f
        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    #-}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    {-# LAWS
        1. join . returnJoin      ≡ id
        2. join . fmap returnJoin ≡ id
        3. join . fmap join       ≡ join . join
    #-}

instance Monad     m => MonadFish m where 
    returnFish  = return
    f >=> g   = \a -> f a >>= g
    -- 1. f >=> returnFish      ≡ f
    -- f >=> returnFish         ≡
    -- \a -> f a >>= returnFish ≡
    -- \a -> f a >>= return     ≡
    -- \a -> f a >>= return     ≡
    -- \a -> f a                ≡
    -- f
    
instance Monad     m => MonadJoin m where
    returnJoin  = return
    join        = (\a -> a >>= id)  -- ≡ join a = a >>= id 
    -- 1. join . returnJoin       ≡ id
    -- (\a -> a >>=id) . return   ≡
    -- (\a -> return a >>=id)     ≡
    -- (\a -> id a)               ≡
    -- (\a -> a)                  ≡
    -- id                         ≡