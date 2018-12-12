import Utilities
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


count_occurrences :: Ord a => [a] -> Map a Int
count_occurrences xs = Map.fromListWith (+) [(x, 1) | x <- xs]


count_boxes :: (Eq a) => a -> [[a]] -> Int
count_boxes a = length . filter (elem a)
-- count_boxes a xs = sum [1 | x <- xs, elem a x]


solve_A :: [[Char]] -> Int
solve_A s = let elems = map (Map.elems . count_occurrences) s in
    (count_boxes 2 elems) * (count_boxes 3 elems)

------------------------------------------------------

strict_zip :: [a] -> [b] -> [(a, b)]
strict_zip [] [] = []
strict_zip (x:xs) (y:ys) = [(x, y)] ++ strict_zip xs ys


-- "abcde" -> "abxde" --> Just "abde"
common_letters :: [Char] -> [Char] -> Maybe [Char]
common_letters xs ys = 
    case [1 | (x, y) <- (strict_zip xs ys), x /= y] of
        []      -> Nothing
        [x]     -> Just [x | (x, y) <- (strict_zip xs ys), x == y]
        xs      -> Nothing


make_pairs :: [a] -> [(a, a)]
make_pairs [] = []
make_pairs (x:xs) = [(x, y) | y <- xs] ++ make_pairs xs


find_match :: [[Char]] -> [[Char]]
find_match s = 
    let pairs = make_pairs s
        commons = [common_letters x y | (x, y) <- pairs] in
    [x | Just x <- commons]


solve_B :: [[Char]] -> [Char]
solve_B s = let [x] = find_match s in x

-----------------------------------------------------

main = do
    s <- readFile "02_input.txt"
    -- let s1 = "fghij"
    -- let s2 = "fguij"
    -- putStrLn (show (common_letters s1 s2 (count_diff s1 s2)))
    let lines = split '\n' s
    putStrLn (show (solve_A lines))
    putStrLn (show (solve_B lines))
