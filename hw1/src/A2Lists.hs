module A2Lists where

import           Control.Arrow ((***))
import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

removeAt :: Int -> [t] -> [t]
removeAt n l = snd $ removeAtHard n l

removeAtHard :: Int -> [t] -> (Maybe t, [t])
removeAtHard n l
  | null l        = (Nothing, l)
  | n < 0         = (Nothing, l)
  | n >= length l = (Nothing, l)
  | otherwise     = (\s -> (Just $ head s, take n l ++ drop 1 s)) $ drop n l

collectEvery :: Int -> [t] -> ([t], [t])
collectEvery _ [] = ([], [])
collectEvery n l
  | n <= 0 = (l, [])
  | otherwise = ((++) (take (n - 1) l) *** (++) (take 1 d)) $ collectEvery n (drop 1 d)
    where
      d = drop (n - 1) l

stringSum :: String -> Integer
stringSum str = sum $ map (\t -> if null t then 0 else read t) $ words str

stringSumHard :: String -> Integer
stringSumHard str = sum $ map getnum $ words str
  where
  getnum :: String -> Integer
  getnum [] = 0
  getnum ('+':str1) = if (read str1 :: Integer) > 0 then read str1 else read $ '+' : str1
  getnum str1 = read str1

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort t = merge (mergeSort $ take halfL t) (mergeSort $ drop halfL t)
  where
  halfL = div (length t) 2
  merge :: (Ord a) => [a] -> [a] -> [a]
  merge [] t1         = t1
  merge t1 []         = t1
  merge (z:zs) (x:xs) = if z < x then z:merge zs (x:xs) else x:merge (z:zs) xs
