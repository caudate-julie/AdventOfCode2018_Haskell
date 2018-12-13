import Utilities
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set


maximum_by_key :: Ord b => (a -> b) -> [a] -> a
maximum_by_key key [x] = x
maximum_by_key key (x:xs) = 
    let xm = maximum_by_key key xs in
    if (key xm) > (key x) then xm else x


strict_minimum_by_key :: Ord b => (a -> b) -> [a] -> Maybe a
strict_minimum_by_key key [x] = Just x
strict_minimum_by_key key (x:xs) =
    case strict_minimum_by_key key xs of
        Nothing     -> Just x
        Just y      -> if (key x) < (key y)       then Just x
                       else if (key x) > (key y)  then Just y
                       else                       Nothing


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


closest :: (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
closest p towns = strict_minimum_by_key (dist p) towns


regions :: (Int, Int, Int, Int) -> [(Int, Int)] -> Map (Int, Int) [(Int, Int)]
regions (x1, y1, x2, y2) towns = 
    let ls = [(town, [(x, y)]) 
                | x <- [x1..x2]
                , y <- [y1..y2]
                , Just town <- [closest (x, y) towns]]
    in Map.fromListWith (++) ls


infinites :: (Int, Int, Int, Int) -> [(Int, Int)] -> Set (Int, Int)
infinites (x1, y1, x2, y2) towns = Set.fromList onbound
    where 
        onbound = [c | x <- [x1..x2], Just c <- [closest (x, y1) towns]]
               ++ [c | x <- [x1..x2], Just c <- [closest (x, y2) towns]]
               ++ [c | y <- [y1..y2], Just c <- [closest (x1, y) towns]]
               ++ [c | y <- [y1..y2], Just c <- [closest (x2, y) towns]]


-- most_dangerous :: (Int, Int, Int, Int) -> [(Int, Int)] -> (Int, Int)
-- most_dangerous bounds towns = 
--     let rs = regions bounds towns
--         onbound = infinites bounds towns
--         inbound = [r | r <- (Map.keys rs), Set.notMember r onbound]
--     in
--         maximum_by_key (length . ((Map.!) rs)) inbound


most_dangerous :: (Int, Int, Int, Int) -> [(Int, Int)] -> Int
most_dangerous bounds towns = 
    let rs = regions bounds towns
        onbound = infinites bounds towns
        inbound = [r | r <- towns, Set.notMember r onbound]    
    in
        maximum [length (rs Map.! r) | r <- inbound]


solve_A :: [[Char]] -> Int
solve_A lines = 
    let coords = get_coordinates lines
        bounds = get_boundaries coords
    in most_dangerous bounds coords


main = do
    s <- readFile "06_input.txt"
    let lines = split '\n' s
    putStrLn (show (solve_A lines))