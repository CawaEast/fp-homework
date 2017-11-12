module B2NonDetMath where


bin :: Int -> [[Int]]
bin 0 = [[]]
bin a = bin (a - 1) >>= \l -> [0:l, 1:l]

combinations:: Int -> Int -> [[Int]]
combinations c n = gen_comb 1 c n
  where
    gen_comb:: Int -> Int -> Int -> [[Int]]
    gen_comb a b l
      | a > b           = [[]]
      | b - a + 1 == l  = [[a .. b]]
      | l == 0          = [[]]
      | otherwise       = (gen_comb (a + 1) b (l - 1) >>= \s -> [a:s]) ++ (gen_comb (a + 1) b l) 
      
permutations :: [a] -> [[a]]
permutations l = perm l
  where 
    le = length l
    perm :: [a] -> [[a]]
    perm (x:xs) = permutations xs >>= \q -> [x:q] >>= (wheel le)
    perm [] = [[]]
    wheel :: Int -> [a] -> [[a]]
    wheel 0 _ = []
    wheel n (x:xs) = ((x:xs):(wheel (n - 1) (xs ++ [x])))
    wheel _ [] = [[]]