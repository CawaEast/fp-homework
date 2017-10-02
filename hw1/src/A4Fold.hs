module A4Fold where

import           TreePrinters (Tree (..))

instance Foldable Tree where
    foldMap _ Leaf         = mempty
    foldMap f (Node k l r) = foldMap f l `mappend` f k `mappend` foldMap f r
    foldr _ z Leaf         = z
    foldr f z (Node k l r) = foldr f (f k (foldr f z r)) l

toList :: (Ord a) => Tree a -> [a]
toList = foldMap (: [])

splitOn :: Char -> String -> [String]
splitOn c str = fst $ foldl (\p ic -> if ic == c then (fst p ++ [snd p], [])
                                          else (fst p, snd p ++ [ic])) ([],[]) (str ++ [c])

joinWith :: Char -> [String] -> String
joinWith c = foldl1 (\a b -> a ++ [c] ++ b)



