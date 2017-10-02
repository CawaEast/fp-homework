module A1Simple where

import           Data.List

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z)
    where
        [x, y, z] = sort [a, b, c]

highestBit :: (Integral a) => a -> (a, a)
highestBit n = if n < 2 then (1, 0) else (fst p * 2, snd p + 1)
    where
        p = highestBit $ div n 2

smartReplicate :: [Int] -> [Int]
smartReplicate []     = []
smartReplicate (b:xs) = replicate b b ++ smartReplicate xs

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains n l = filter (elem n) l
