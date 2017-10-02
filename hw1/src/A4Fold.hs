module A4Fold where

import           Tree (Tree (..))

--implemented in Tree.hs
--instance Foldable Tree where

toList :: (Ord a) => Tree a -> [a]
toList = foldMap (: [])

splitOn :: Char -> String -> [String]
splitOn c str = fst $ foldl (\p ic -> if ic == c then (fst p ++ [snd p], [])
                                          else (fst p, snd p ++ [ic])) ([],[]) (str ++ [c])

joinWith :: Char -> [String] -> String
joinWith c = foldl1 (\a b -> a ++ [c] ++ b)



