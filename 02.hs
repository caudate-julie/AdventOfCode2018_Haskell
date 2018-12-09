import Utilities
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


increment :: Maybe Int -> Int
increment Nothing = 1
increment (Just x) = x + 1


count_all :: Ord a => [a] -> Map a Int
count_all [] = Map.empty
count_all (x:xs) = 
    let m = count_all xs
        v = Map.lookup x m in
    Map.insert x (increment v) m


count_boxes :: Int -> [[Int]] -> Int
count_boxes a xs = sum [1 | x <- xs, elem a x]


solve_A :: [[Char]] -> Int
solve_A s = let elems = map (Map.elems . count_all) s in
    (count_boxes 2 elems) * (count_boxes 3 elems)

------------------------------------------------------

strict_zip :: [a] -> [b] -> [(a, b)]
strict_zip [] [] = []
strict_zip (x:xs) (y:ys) = [(x, y)] ++ strict_zip xs ys


common_letters :: [Char] -> [Char] -> Maybe [Char]
common_letters xs ys = 
    case [1 | (x, y) <- (strict_zip xs ys), x /= y] of
        []      -> Nothing
        [x]     -> Just [x | (x, y) <- (strict_zip xs ys), x == y]
        xs      -> Nothing


find_match_by_string :: [[Char]] -> [Char] -> Maybe [Char]
find_match_by_string [] s = Nothing
find_match_by_string (x:xs) s = 
    case (common_letters x s) of 
        Nothing     -> find_match_by_string xs s
        x           -> x


find_match :: [[Char]] -> Maybe [Char]
find_match [] = Nothing
find_match (x:xs)= 
    let match = find_match_by_string xs x in
    if match == Nothing 
    then find_match xs
    else match


solve_B :: [[Char]] -> [Char]
solve_B s = let Just x = find_match s in x

-----------------------------------------------------

main = do
    s <- readFile "02_input.txt"
    -- let s1 = "fghij"
    -- let s2 = "fguij"
    -- putStrLn (show (common_letters s1 s2 (count_diff s1 s2)))
    let lines = split '\n' s
    putStrLn (show (solve_A lines))
    putStrLn (show (solve_B lines))
