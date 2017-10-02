module A3Types where

import           Data.List (sort)
import           Tree      (Tree (..))

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
                   deriving (Eq, Ord, Show, Read, Bounded, Enum)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Sunday = Monday
nextDay x      = succ x

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day n = toEnum $ (n + fromEnum day) `mod` 7

isWeekend :: DayOfWeek -> Bool
isWeekend day = fromEnum day > 4


class Fighter e where
  hp      :: e -> Int
  ad      :: e -> Int
  name    :: e -> String
  isBrave :: e -> Bool

data Knight = Knight String Int Int

instance Fighter Knight where
  hp      (Knight _ h _) = h
  ad      (Knight _ _ a) = a
  name    (Knight n _ _) = n
  isBrave _              = True

data Monster = Monster Int Int

instance Fighter Monster where
  hp      (Monster h _) = h
  ad      (Monster _ a) = a
  name    (Monster _ _) = "Name??? Just monster."
  isBrave _             = False

fight :: (Fighter tf1, Fighter tf2) => tf1 -> tf2 -> (Either tf1 tf2, Int)
fight f1 f2 = checkLast f1 f2
  where
    hp2b = if isBrave f2 && not (isBrave f1) then hp f2 + ad f1 else hp f2
    initR = min (div (hp f1) (ad f2)) (div hp2b (ad f1))
    hp1 = hp f1 - initR * ad f2
    hp2 = hp2b  - initR * ad f1
    checkLast :: (Fighter tf1, Fighter tf2) => tf1 -> tf2 -> (Either tf1 tf2, Int)
    checkLast ff1 ff2
      | hp2 == 0      = (Left ff1, initR)
      | hp1 == 0      = (Right ff2, initR)
      | hp2 <= ad f1  = (Left ff1, initR + 1)
      | otherwise     = (Right ff2, initR + 1)


data Vector a = Vector2D a a | Vector3D a a a deriving (Read, Eq, Ord, Show)

getX :: Vector a -> a
getX (Vector2D a _)   = a
getX (Vector3D a _ _) = a

getY :: Vector a -> a
getY (Vector2D _ b)   = b
getY (Vector3D _ b _) = b

getZ :: (Num a) => Vector a -> a
getZ (Vector2D _ _)   = 0
getZ (Vector3D _ _ c) = c

instance (Num a) => Num (Vector a) where
  (Vector2D a b) + (Vector2D c d) = Vector2D (a + c)           (b + d)
  a + b                           = Vector3D (getX a + getX b) (getY a + getY b) (getZ a + getZ b)
  negate (Vector2D a b)           = Vector2D (negate a)        (negate b)
  negate (Vector3D a b c)         = Vector3D (negate a)        (negate b)        (negate c)
  a * b                           = Vector3D ( getY a * getZ b - getZ a * getY b )
                                             (-getX a * getZ b + getZ a * getX b)
                                             ( getX a * getY b - getY a * getX b)
  abs v                           = Vector3D (abs $ getX v)    (abs $ getY v)    (abs $ getZ v)
  signum v                        = Vector3D (signum $ getX v) (signum $ getY v) (signum $ getZ v)
  fromInteger v                   = Vector2D (fromInteger v)   0

scalar :: (Num a) => Vector a -> Vector a -> a
scalar a b = (getX a * getX b) + (getY a * getY b) + (getZ a * getZ b)

vectorLength :: (Floating a) => Vector a -> a
vectorLength a = sqrt (scalar a a)

dist :: (Floating a) => Vector a -> Vector a -> a
dist a b = vectorLength (a - b)


data Nat = Z | S Nat deriving (Show)

instance Eq Nat where
  (==) Z     Z     = True
  (==) (S a) (S b) = a == b
  (==) _     _     = False

instance Ord Nat where
  (<=) Z     _     = True
  (<=) (S a) (S b) = a <= b
  (<=) _     _     = False

instance Num Nat where
  (+) a Z     = a
  (+) a (S b) = S a + b
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
  fromEnum (S a) = fromEnum a + 1
  fromEnum Z     = 0
  toEnum         = consructToNat toEnum

instance Integral Nat where
  quotRem _ Z     = (0, 0)
  quotRem a b     = if a < b then (Z, a) else (\ p ->(S $ fst p, snd p)) $ quotRem (a - b) b
  divMod          = quotRem
  toInteger (S a) = toInteger a + 1
  toInteger Z     = 0

consructToNat :: (Num t, Ord t) => (t -> Nat) -> (t -> Nat)
consructToNat func a = if a > 0 then S (func (a - 1)) else Z

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S a)) = isEven a

gcdNat :: Nat -> Nat -> Nat
gcdNat Z _    = Z
gcdNat _ Z    = Z
gcdNat a b
  | a == b    = a
  | a > b     = gcd (a - b) b
  | otherwise = gcd a (b - a)

--data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Functor, Show)

isTreeEmpty :: Tree a -> Bool
isTreeEmpty Leaf = True
isTreeEmpty _    = False

countTreeElems :: Tree a -> Int
countTreeElems Leaf         = 0
countTreeElems (Node _ b c) = 1 + countTreeElems b + countTreeElems c

containsTree :: (Ord a) => Tree a -> a -> Bool
containsTree Leaf _ = False
containsTree (Node a b c) e
  | a == e          = True
  | e < a           = containsTree b e
  | otherwise       = containsTree c e

putTree :: (Ord a) => Tree a -> a -> Tree a
putTree Leaf a = Node a Leaf Leaf
putTree (Node a b c) e
  | a == e    = Node a b c
  | e < a     = Node a (putTree b e) c
  | otherwise = Node a b (putTree c e)

fromList :: (Ord a) => [a] -> Tree a
fromList t = sortedToList $ sort t
  where
    sortedToList :: (Ord a) => [a] -> Tree a
    sortedToList []  = Leaf
    sortedToList [a] = Node a Leaf Leaf
    sortedToList t2  = (\l -> Node (t2 !! l) (sortedToList $ take l t2) (sortedToList $ drop (l + 1) t2)) $ div (length t2) 2
