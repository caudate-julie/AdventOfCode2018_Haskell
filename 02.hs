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


main = do
    s <- readFile "02_input.txt"
    putStrLn (show (solve_A (split '\n' s)))
