import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete)
import Utilities


-- Step C must be finished before step P can begin.
parse_instructions :: [[Char]] -> [(Char, Char)]
parse_instructions lines = map get_instruction lines
    where
        get_instruction :: [Char] -> (Char, Char)
        get_instruction line = (line!!5, line!!36)


create_graph :: [(Char, Char)] -> Map Char [Char]
create_graph instructions = Map.fromList [(x, []) | (f, s) <- instructions, x <- [f, s]]


fill_graph :: Map Char [Char] -> [(Char, Char)] -> Map Char [Char]
fill_graph m [] = m
fill_graph m ((pre, post):xs) = 
    let m' = fill_graph m xs
    in Map.insertWith (++) post [pre] m'


next_instruction :: [(Char, [Char])] -> Char
next_instruction ((c, []):xs) = c
next_instruction (x:xs) = next_instruction xs


carry_instruction :: Char -> [(Char, [Char])] -> [(Char, [Char])]
carry_instruction c xs =
    let xs' = delete (c, []) xs 
    in
        [(t, delete c ts) | (t, ts) <- xs']


instruction_sequence :: [(Char, [Char])] -> [Char]
instruction_sequence [] = []
instruction_sequence xs =
    let c = next_instruction xs
        xs' = carry_instruction c xs
    in
        [c] ++ instruction_sequence xs'


solve_A lines =
    let instructions = parse_instructions lines 
        graph = fill_graph (create_graph instructions) instructions
        list = Map.toAscList graph
    in instruction_sequence list


main = do
    s <- readFile ("07_input.txt")
    let lines = split '\n' s
    let line = "Step C must be finished before step P can begin."
    putStrLn (show (solve_A lines))