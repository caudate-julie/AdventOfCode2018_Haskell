import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete)
import Data.List (minimumBy)
import Utilities
import Data.Char (ord)


-- Step C must be finished before step P can begin.
parse_instructions :: [[Char]] -> [(Char, Char)]
parse_instructions lines = map get_instruction lines
    where
        get_instruction :: [Char] -> (Char, Char)
        get_instruction line = (line!!5, line!!36)


-- All letters that appear in instructions are gathered to a singular list
create_empty_graph :: [(Char, Char)] -> Map Char [Char]
create_empty_graph instructions = Map.fromList [(x, []) | (f, s) <- instructions, x <- [f, s]]


fill_graph :: Map Char [Char] -> [(Char, Char)] -> Map Char [Char]
fill_graph m [] = m
fill_graph m ((pre, post):xs) = 
    let m' = fill_graph m xs
    in Map.insertWith (++) post [pre] m'

-- then map will be turned into alphabetic list of pairs.

next_instruction :: [(Char, [Char])] -> Char
next_instruction ((c, []):xs) = c
next_instruction (x:xs) = next_instruction xs


carry_instruction :: Char -> [(Char, [Char])] -> [(Char, [Char])]
carry_instruction c xs = [(t, delete c ts) | (t, ts) <- xs]


instruction_sequence :: [(Char, [Char])] -> [Char]
instruction_sequence [] = []
instruction_sequence xs =
    let c = next_instruction xs
        xs' = delete (c, []) (carry_instruction c xs)
    in
        [c] ++ instruction_sequence xs'


solve_A lines =
    let instructions = parse_instructions lines 
        graph = fill_graph (create_empty_graph instructions) instructions
        list = Map.toAscList graph
    in instruction_sequence list

--------------------------------------------------------

-- ongoing is [(Char, Int)] : [(instruction, finish time)]


runtime :: Char -> Int
runtime instruction = (a_time + ord(instruction) - ord('A'))

-- current time -> ongoing -> pending instructions -> new ongoing
add_timed_labors :: Int -> [(Char, Int)] -> [(Char, [Char])] -> [(Char, Int)]
add_timed_labors time ongoing [] = ongoing
add_timed_labors time ongoing ((c, []):xs) = 
    if length ongoing == workers_amount
    then ongoing
    else add_timed_labors time (ongoing ++ [(c, time + runtime c)]) xs
add_timed_labors time ongoing (x:xs) = add_timed_labors time ongoing xs


delete_running :: [(Char, Int)] -> [(Char, [Char])] -> [(Char, [Char])]
delete_running ongoing instructions = 
    let list = [c | (c, t) <- ongoing]
    in filter (\(c, xs) -> notElem c list) instructions


next_time_point :: [(Char, Int)] -> Int
next_time_point ongoing = 
    let comp (cx, tx) (cy, ty) = compare tx ty
        (c, time) = minimumBy comp ongoing
    in time


-- ongoing -> pending instructions -> (next time point, still ongoing, still pending)
time_flies :: [(Char, Int)] -> [(Char, [Char])] -> (Int, [(Char, Int)], [(Char, [Char])])
time_flies ongoing instructions =
    let time = next_time_point ongoing
        finished = [c | (c, t) <- ongoing, t == time]
        updated_instructions = foldr carry_instruction instructions finished
        updated_ongoing = [(c, t) | (c, t) <- ongoing, notElem c finished]
    in (time, updated_ongoing, updated_instructions)


time_cycle :: (Int, [(Char, Int)], [(Char, [Char])]) -> Int
time_cycle (time, ongoing, instructions) = 
    let updated_ongoing = add_timed_labors time ongoing instructions
        updated_instructions = delete_running updated_ongoing instructions
    in case updated_ongoing of
        [] -> time
        xs -> let next_state = time_flies updated_ongoing updated_instructions
              in time_cycle next_state


solve_B lines = 
    let instructions = parse_instructions lines 
        graph = fill_graph (create_empty_graph instructions) instructions
        list = Map.toAscList graph
    in time_cycle (0, [], list)


--------------------------------------------------------

workers_amount = 5
a_time = 61

main = do
    s <- readFile ("07_input.txt")
    let lines = split '\n' s
    putStrLn (show (solve_B lines))