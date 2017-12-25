{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes    #-}


module TH_L1 where

import Language.Haskell.TH
-- import Data.Sequence(replicateM)
import Control.Monad(replicateM)
import Data.Functor.Identity
import Data.Functor.Const
import Data.Either(either)
import Control.Arrow((***))
import qualified Data.Text as T

fst3 = do
    x <- newName "x"
    lamE [tupP [varP x, wildP, wildP]] (varE x)
    
tuple :: Int -> ExpQ
tuple n = do
    ns <- replicateM n (newName "x")
    lamE [foldr (\x y -> conP '(:) [varP x,y]) wildP ns] (tupE $ map varE ns)
    
fstN :: Int   -- ^ take tuple «length» as argument
     -> Q Exp
fstN n = do
    x <- newName "x"
    lamE [tupP $ varP x : replicate (n - 1) wildP] (varE x)
    
chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n lst = do
    vars <- replicateM n (newName "x")
    lamE [tupP $ map varP vars] (tupE $ map (\i -> varE $ vars!!i) lst)
    
class ShowText e where
    showText:: e -> T.Text
    
makeShowText :: Name -> Q [Dec]
makeShowText name = do
    let nameStr = nameBase name
    [d|instance ShowText $(conT name) where showText a = T.pack nameStr|]

 
type Lens obj obj' field field' = forall f . Functor f => (field -> f field') -> obj -> f obj' 
 
type Lens' obj field  = Lens obj obj field field

set  :: Lens s t a b  -> b -> s -> t         -- set    value (setter)
set l a s = runIdentity $ l (\_ -> Identity a) s

view :: Lens s t a b -> s -> a            -- lookup value (getter)
view l s = getConst $ l (\a -> Const a) s

over :: Lens s t a b -> (a -> b) -> s -> t  -- change value (modifier)
over l f s = runIdentity $ l (\a -> Identity $ f a) s

(.~) :: Lens' s a -> a -> s -> s
(^.) :: s -> Lens' s a -> a
(%~) :: Lens' s a -> (a -> a) -> s -> s
(.~) = set
(^.) s l = view l s
(%~) = over

_1 :: Lens (a, x) (b, x) a b
_1 f (a, b) = (\q -> (q, b)) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (a, b) = (\q -> (a, q)) <$> f b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = (\f_arg s0 -> set s0 <$> (f_arg $ get s0))

choosing :: Lens s1 t1 a b 
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (\s    -> either (view l1) (view l2) s) 
                      (\s0 b -> either (Left . set l1 b) (Right . set l2 b) s0)
                                                                   
-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (res, set l res s)
    where
        res = f $ view l s
        
-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (res, set l (f res) s)
    where
        res = view l s