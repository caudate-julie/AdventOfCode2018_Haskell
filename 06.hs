import Utilities
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set


get_coordinates :: [[Char]] -> [(Int, Int)]
get_coordinates lines = [(x, y) | line <- lines,
                                  let [x, y] = map read (split ',' line)]


get_boundaries :: [(Int, Int)] -> (Int, Int, Int, Int)
get_boundaries coords = 
    let x1 = minimum [x | (x, _) <- coords]
        x2 = maximum [x | (x, _) <- coords]
        y1 = minimum [y | (_, y) <- coords]
        y2 = maximum [y | (_, y) <- coords]
    in (x1 - 1, y1 - 1, x2 + 1, y2 + 1)


dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


closest :: (Int, Int) -> [(Int, Int)] -> Uniquable (Int, Int)
closest p towns = foldl (strict_min_by_key (dist p)) Empty towns


regions :: (Int, Int, Int, Int) -> [(Int, Int)] -> Map (Int, Int) [(Int, Int)]
regions (x1, y1, x2, y2) towns = 
    let ls = [(town, [(x, y)]) 
                | x <- [x1..x2]
                , y <- [y1..y2]
                , Single town <- [closest (x, y) towns]]
    in Map.fromListWith (++) ls


infinites :: (Int, Int, Int, Int) -> [(Int, Int)] -> Set (Int, Int)
infinites (x1, y1, x2, y2) towns = Set.fromList onbound
    where 
        onbound = [c | x <- [x1..x2], y <- [y1, y2], Single c <- [closest (x, y) towns]]
               ++ [c | x <- [x1, x2], y <- [y1..y2], Single c <- [closest (x, y) towns]]


most_dangerous :: (Int, Int, Int, Int) -> [(Int, Int)] -> Int
most_dangerous bounds towns = 
    let rs = regions bounds towns
        onbound = infinites bounds towns 
    in
        maximum [length (rs Map.! r) | r <- towns, Set.notMember r onbound]


solve_A :: [[Char]] -> Int
solve_A lines = 
    let coords = get_coordinates lines
        bounds = get_boundaries coords
    in most_dangerous bounds coords

--------------------------------------------------------

in_range_of :: Int -> (Int, Int, Int, Int) -> [(Int, Int)] -> Int
in_range_of max_dist (x1, y1, x2, y2) towns = 
    let dists = [sum [dist (x, y) t | t <- towns] 
                     | x <- [x1..x2], y <- [y1..y2]]
    in sum[1 | d <- dists, d < max_dist]


solve_B :: Int -> [[Char]] -> Int
solve_B max_dist lines =
    let coords = get_coordinates lines
        bounds = get_boundaries coords
    in in_range_of max_dist bounds coords

--------------------------------------------------------

main = do
    s <- readFile "06_input.txt"
    let lines = split '\n' s
    putStrLn (show (solve_A lines))
    putStrLn (show (solve_B 10000 lines))