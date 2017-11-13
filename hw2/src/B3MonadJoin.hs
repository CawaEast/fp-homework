{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B3MonadJoin where

--import Prelude (id)
--import B3Monad(Monad)
--import B3MonadFish(MonadFish)
class Functor f where
    fmap :: (a -> b) -> f a -> f b

    --{-# LAWS
    --    1. fmap id         ≡ id
    --    2. fmap f . fmap g ≡ fmap (f . g)
    

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    --{-# LAWS
    --    1. m >>= return    ≡ m
    --    2. return a >>= f  ≡ f a
    --    3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    --{-# LAWS
    --    1. f >=> returnFish ≡ f
    --    2. returnFish >=> f ≡ f
    --    3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    --{-# LAWS
    --    1. join . returnJoin      ≡ id
    --    2. join . fmap returnJoin ≡ id
    --    3. join . fmap join       ≡ join . join
    

    
instance (Functor m, MonadJoin m) => Monad     m where 
    return = returnJoin
    a >>= f = join (fmap f a) 
    
    -- 1. m >>= return          ≡ m
    -- join (fmap returnJoin m) ≡
    -- id m                     ≡
    -- m                        ≡
    
instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    f >=> g = \a -> join(fmap g (f a)) 
    
    -- 1. f >=> returnFish              ≡ f
    -- \a -> join(fmap returnJoin (f a))≡
    -- \a -> id (f a)                   ≡
    -- \a -> f a                        ≡
    -- f                                ≡