{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes    #-}


module TH_L1(chooseByIndices, makeShowText, ShowText(..), set, over, view, lens, choosing, (<%~), (<<%~), _1, _2) where

import Language.Haskell.TH(lamE, Q, varP, varE, tupE, Name, Dec, Exp, conT, nameBase, tupP, newName)
import Control.Monad(replicateM)
import Data.Functor.Identity(Identity(..), runIdentity)
import Data.Functor.Const(Const(..), getConst)
import Data.Either(either)
import qualified Data.Text as T(Text, pack)
    
chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n lst = do
    vars <- replicateM n (newName "x")
    lamE [tupP $ map varP vars] (tupE $ map (\i -> varE $ vars!!i) lst)
    
class ShowText e where
    showText:: e -> T.Text
    
makeShowText :: Name -> Q [Dec]
makeShowText name = do
    let nameStr = nameBase name
    [d|instance ShowText $(conT name) where showText _ = T.pack nameStr|]

 
type Lens obj obj' field field' = forall f . Functor f => (field -> f field') -> obj -> f obj' 
 
--type Lens' obj field  = Lens obj obj field field

set  :: Lens s t a b  -> b -> s -> t         -- set    value (setter)
set l a s = runIdentity $ l (\_ -> Identity a) s

view :: Lens s t a b -> s -> a            -- lookup value (getter)
view l s = getConst $ l Const s

over :: Lens s t a b -> (a -> b) -> s -> t  -- change value (modifier)
over l f s = runIdentity $ l (Identity . f) s

--(.~) :: Lens' s a -> a -> s -> s
--(^.) :: s -> Lens' s a -> a
--(%~) :: Lens' s a -> (a -> a) -> s -> s
--(.~) = set
--(^.) s l = view l s
--(%~) = over

_1 :: Lens (a, x) (b, x) a b
_1 f (a, b) = (\q -> (q, b)) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (a, b) = (\q -> (a, q)) <$> f b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set1  f_arg s0 = set1 s0 <$> f_arg (get s0)

choosing :: Lens s1 t1 a b 
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (view l1) (view l2)) 
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