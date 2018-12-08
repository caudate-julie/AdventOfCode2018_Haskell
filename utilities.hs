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
