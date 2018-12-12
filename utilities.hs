module Utilities where

findChar :: (Eq a) => a -> [a] -> Int
findChar c [] = 0
findChar c (x:xs) = if (x == c) 
    then 0
    else (findChar c xs) + 1


split :: (Eq a) => a -> [a] -> [[a]]
split c [] = []
split c s = 
    let n = findChar c s in
    [take n s] ++ split c (drop (n+1) s)


sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [y | y <- (x:xs), y == x] ++ sort [y | y <- xs, y > x]


-- slice '?' '!' "ab?def!g" = "def"
-- slice '?' '!' "ab?defg" = "defg"
slice :: (Eq a) => a -> a -> [a] -> [a]
slice x y s = 
    let ix = findChar x s
        s1 = drop (ix+1) s
        iy = findChar y s1 in
    take iy s1