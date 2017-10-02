module A1Simple where

import           Control.Arrow ((***))
import           Data.List     (sort)

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (\ [z, x, y] -> (z, x, y)) $ sort [a, b, c]

highestBit :: (Integral a) => a -> a
highestBit a = fst $ highestBitHard a

highestBitHard :: (Integral a) => a -> (a, a)
highestBitHard n = if n < 2 then (1, 0) else ((*) 2 *** (+) 1) $ highestBitHard $ div n 2

smartReplicate :: [Int] -> [Int]
smartReplicate = foldr (\ b -> (++) (replicate b b)) []

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains n = filter (elem n)
