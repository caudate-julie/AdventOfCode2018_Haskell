import Utilities
-- import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


increment :: Maybe Int -> Int
increment Nothing = 1
increment (Just x) = x + 1


count_all :: Ord a => [a] -> Map a Int
count_all [] = Map.empty
count_all (x:xs) = 
    let m = count_all xs in
    let v = Map.lookup x m in
    Map.insert x (increment v) m

count_boxes :: [[Char]] -> (Int, Int)
count_boxes [] = (0, 0)
count_boxes (x:xs) = let (twos, threes) = count_boxes xs in
    let m = count_all x in
    let els = Map.elems m in
    (if (elem 2 els) then twos + 1 else twos,
     if (elem 3 els) then threes + 1 else threes)


solve_A :: [[Char]] -> Int
solve_A s = let (twos, threes) = count_boxes s in twos * threes


foo = count_all "aaabbc"
    

main = do
    s <- readFile "02_input.txt"
    -- let s = "+3\n+3\n+4\n-2\n-4\n"
    putStrLn (show (solve_A (split '\n' s)))               -- show ~ repr
    -- putStrLn(show(foo))



-- \x y -> ...     lambdas