{-# LANGUAGE RecordWildCards #-}

module A3Types where

import           TreePrinters (Tree (..))

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
                   deriving (Eq, Ord, Show, Read, Bounded, Enum)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Sunday = Monday
nextDay x      = succ x

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day n = toEnum $ (n + fromEnum day) `mod` 7

isWeekend :: DayOfWeek -> Bool
isWeekend day = fromEnum day > 4


data Fighter
    = Knight{name :: String, hp :: Int, ad :: Int}
    | Monster{hp :: Int, ad :: Int}

getHp :: Fighter -> Int
getHp Knight{hp = t, ..}  = t
getHp Monster{hp = t, ..} = t

getAd :: Fighter -> Int
getAd Knight{ad = t, ..}  = t
getAd Monster{ad = t, ..} = t

getName :: Fighter -> String
getName Knight{name = n, ..} = n
getName Monster{..}          = "Name??? Just monster."

fight :: Fighter -> Fighter -> Fighter
fight f1 f2 = checkLast rHp1 rHp2
    where
        initR = min (div (getHp f1) (getAd f2)) (div (getHp f2) (getAd f1))
        rHp1 = (getHp f1) - (initR * getAd f2)
        rHp2 = (getHp f2) - (initR * getAd f1)
        checkLast :: Int -> Int -> Fighter
        checkLast hp1 hp2
            | hp2 == 0          = f1
            | hp1 == 0          = f2
            | hp2 <= getAd f1   = f1
            | otherwise         = f2


data Vector a = Vector2D a a | Vector3D a a a deriving (Read, Eq, Ord, Show)

getX :: (Vector a) -> a
getX (Vector2D a _)   = a
getX (Vector3D a _ _) = a

getY :: (Vector a) -> a
getY (Vector2D _ b)   = b
getY (Vector3D _ b _) = b

getZ :: (Num a) => (Vector a) -> a
getZ (Vector2D _ _)   = 0
getZ (Vector3D _ _ c) = c

instance (Num a) => Num (Vector a) where
    (Vector2D a b) + (Vector2D c d) = (Vector2D (a + c) (b + d))
    a + b                           = (Vector3D (getX a + getX b) (getY a + getY b) (getZ a + getZ b))
    negate (Vector2D a b)   = (Vector2D (negate a) (negate b))
    negate (Vector3D a b c) = (Vector3D (negate a) (negate b) (negate c))
    a * b                           = (Vector3D (getY a * getZ b - getZ a * getY b )
                                                (-getX a * getZ b + getZ a * getX b)
                                                (getX a * getY b - getY a * getX b))
    abs v                           = (Vector3D (abs $ getX v) (abs $ getY v) (abs $ getZ v))
    signum v                        = (Vector3D (signum $ getX v) (signum $ getY v) (signum $ getZ v))
    fromInteger v                   = (Vector2D (fromInteger v) 0)

scalar :: (Num a) => (Vector a) -> (Vector a) -> a
scalar a b = (getX a * getX b) + (getY a * getY b) + (getZ a * getZ b)

vectorLength :: (Floating a) => (Vector a) -> a
vectorLength a = sqrt (scalar a a)

dist :: (Floating a) => (Vector a) -> (Vector a) -> a
dist a b = vectorLength (a - b)



data Nat = Z | S Nat deriving (Show)

instance Eq Nat where
    (==) Z Z         = True
    (==) (S a) (S b) = a == b
    (==) _ _         = False

instance Ord Nat where
    (<=) Z _         = True
    (<=) (S a) (S b) = a <= b
    (<=) _ _         = False

instance Num Nat where
    (+) a Z     = a
    (+) a (S b) = (S a) + b
    (*) _ Z     = Z
    (*) a (S b) = a * b + a
    (-) a Z         = a
    (-) Z _         = Z
    (-) (S a) (S b) = a - b
    abs a           = a
    fromInteger  = consructToNat fromInteger
    signum Z = 0
    signum _ = 1

instance Real Nat where
    toRational a = toRational (toInteger a)

instance Enum Nat where
    pred Z     = Z
    pred (S a) = a
    fromEnum (S a) = (fromEnum a) + 1
    fromEnum Z     = 0
    toEnum = consructToNat toEnum

instance Integral Nat where
    quotRem _ Z     = (0, 0)
    quotRem a b     = if a < b then (Z, a) else (\p ->((S $ fst p), snd p)) $ quotRem (a - b) b
    divMod          = quotRem
    toInteger (S a) = (toInteger a) + 1
    toInteger Z     = 0

consructToNat :: (Num t, Ord t) => (t -> Nat) -> (t -> Nat)
consructToNat func = (\a -> if (a > 0) then (S (func (a - 1))) else Z)

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S a)) = isEven a

gcdNat :: Nat -> Nat -> Nat
gcdNat Z _ = Z
gcdNat _ Z = Z
gcdNat a b = if (a == b) then a else if a > b then gcd (a - b) b else gcd a (b - a)

--data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Functor, Show)

isTreeEmpty :: (Tree a) -> Bool
isTreeEmpty Leaf = True
isTreeEmpty _    = False

countTreeElems :: (Tree a) -> Int
countTreeElems Leaf         = 0
countTreeElems (Node _ b c) = 1 + countTreeElems b + countTreeElems c

containsTree :: (Ord a) => (Tree a) -> a -> Bool
containsTree Leaf _ = False
containsTree (Node a b c) e = if (a == e) then True else
                                  if e < a then containsTree b e else containsTree c e

putTree :: (Ord a) => (Tree a) -> a -> (Tree a)
putTree Leaf a = Node a Leaf Leaf
putTree (Node a b c) e = if (a == e) then (Node a b c) else
                                  if e < a then (Node a (putTree b e) c)
                                  else (Node a b (putTree c e))

fromList :: (Ord a) => [a] -> (Tree a)
fromList [] = Leaf
fromList [a] = Node a Leaf Leaf
fromList t = Node (t !! l) (fromList (take l t)) (fromList (drop (l + 1) t))
    where
        l = div (length t) 2
