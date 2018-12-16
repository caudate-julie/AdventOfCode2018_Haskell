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


sortBy :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy f [] = []
sortBy f (x:xs) = sortBy f [y | y <- xs, f y < f x]
             ++ [y | y <- (x:xs), f y == f x]
             ++ sortBy f [y | y <- xs, f y > f x]


-- slice '?' '!' "ab?def!g" = "def"
-- slice '?' '!' "ab?defg" = "defg"
slice :: (Eq a) => a -> a -> [a] -> [a]
slice x y s = 
    let ix = findChar x s
        s1 = drop (ix+1) s
        iy = findChar y s1 in
    take iy s1


------------------------------------------
-- by-key comparisons 

min_by_key :: Ord b => (a -> b) -> a -> a -> a
min_by_key key x y = if (key x) < (key y) then x else y


max_by_key :: Ord b => (a -> b) -> a -> a -> a
max_by_key key x y = if (key x) < (key y) then y else x


data Uniquable x = Single x | Multiple x | Empty


strict_min_by_key :: Ord b => (a -> b) -> Uniquable a -> a -> Uniquable a
strict_min_by_key key Empty y = Single y
strict_min_by_key key (Single x) y = 
        case compare (key x) (key y) of
            LT  -> Single x
            GT  -> Single y
            EQ  -> Multiple x
strict_min_by_key key (Multiple x) y = 
        case compare (key x) (key y) of
            LT  -> Multiple x
            GT  -> Single y
            EQ  -> Multiple x


strict_max_by_key :: Ord b => (a -> b) -> Uniquable a -> a -> Uniquable a
strict_max_by_key key Empty y = Single y
strict_max_by_key key (Single x) y = 
        case compare (key x) (key y) of
            LT  -> Single y
            GT  -> Single x
            EQ  -> Multiple x
strict_max_by_key key (Multiple x) y = 
        case compare (key x) (key y) of
            LT  -> Single y
            GT  -> Multiple x
            EQ  -> Multiple x

