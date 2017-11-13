{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module B4Const(Const(..), Functor(..), Applicative(..), Foldable(..), Traversable(..)) where

-- import Prelude ((.), id)
import Data.Monoid(Monoid(..))

newtype Const a b = Const { getConst :: a }

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor (Const a) where
  fmap _ (Const v) = Const v
  
  -- fmap id (Const v) ≡ id (Const v) 
  -- fmap id (Const v) ≡ 
  -- (Const v) ≡
  -- id (Const v) ≡
       
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

        
instance Monoid m => Applicative (Const m) where
  pure _ = Const mempty
  Const f <*> Const v = Const (f `mappend` v)

  
class Foldable t where
  --foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m

  
instance Foldable (Const a) where
  foldMap _ _         = mempty
  
  
class (Functor t, Foldable t) => Traversable t where
    traverse    :: Applicative f => (a -> f b) -> t a -> f (t b)
    --sequenceA   :: Applicative f => t (f a) -> f (t a)
    
instance Traversable (Const a) where
  traverse _ (Const m) = pure (Const m)
   