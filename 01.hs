import Utilities

-- Sum from file

my_read :: [Char] -> Int
my_read s = 
    if s!!0 == '+'
    then my_read (tail s)
    else read s


my_map :: (a -> b) -> [a] -> [b]
my_map f [] = []
my_map f (x:xs) = [f x] ++ my_map f xs   -- ++ for concatenate

---------------------------------------------------------

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce f def [] = def
reduce f def (x:xs) = f x (reduce f def xs)


solve_A :: [Char] -> Int
solve_A s = reduce (+) 0 (my_map my_read (split '\n' s))


---------------------------------------------------------

duplicates :: [Int] -> [Int] -> Int
duplicates (x:xs) y = let ynext = x + head y in
    if (elem ynext y) then ynext else duplicates (xs ++ [x]) ([ynext] ++ y)


solve_B :: [Char] -> Int
solve_B s = duplicates (my_map my_read (split '\n' s)) [0]
    

main = do
    s <- readFile "01_input.txt"
    -- let s = "+3\n+3\n+4\n-2\n-4\n"
    putStrLn (show (solve_A s))               -- show ~ repr
    putStrLn (show (solve_B s))



-- \x y -> ...     lambdas