module A1Simple where

import           Data.List(sort)

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z)
    where
        [x, y, z] = sort [a, b, c]

highestBit :: (Integral a) => a -> a
highestBit a = fst $ highestBitHard a     
        
highestBitHard :: (Integral a) => a -> (a, a)
highestBitHard n = if n < 2 then (1, 0) 
                   else (\ p -> (fst p * 2, snd p + 1)) $ highestBitHard $ div n 2

smartReplicate :: [Int] -> [Int]
smartReplicate [] = []
smartReplicate xs = foldr (\ b -> (++) (replicate b b)) [] xs

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains n = filter (elem n)
