module A2Lists where

import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

removeAt :: Int -> [t] -> (Maybe t, [t])
removeAt n l = (if null s then Nothing else Just $ head s, take n l ++ drop 1 s)
    where
        s = drop n l

collectEvery :: Int -> [t] -> ([t], [t])
collectEvery _ [] = ([], [])
collectEvery n l = (take (n - 1) l ++ fst prevVals, take 1 $ drop (n - 1) l ++ snd prevVals)
    where
        prevVals = collectEvery n $ drop n l

stringSum :: [Char] -> Integer
stringSum str = sum $ map getnum $ words str
    where
    getnum :: [Char] -> Integer
    getnum [] = 0
    getnum ('+':str1) = if (read str1 :: Integer) > 0 then read str1 else read $ ['+'] ++ str1
    getnum str1 = read str1

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort t = merge (mergeSort $ take halfL t) (mergeSort $ drop halfL t)
    where
    halfL = div (length t) 2
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge [] t1 = t1
    merge t1 [] = t1
    merge (z:zs) (x:xs) = if z < x then (z:merge zs (x:xs)) else (x:merge (z:zs) xs)
