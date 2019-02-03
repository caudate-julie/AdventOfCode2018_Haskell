import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete)
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

data State = State { time :: Int
                   , ongoing :: [(Char, Int)]       -- [(instruction, finish time)]
                   , pending :: [(Char, [Char])]    -- [(instruction, dependencies)]
                   }


runtime :: Char -> Int
runtime instruction = (a_time + ord(instruction) - ord('A'))


next_time_point :: [(Char, Int)] -> Int
next_time_point ongoing = minimum [t | (c, t) <- ongoing]


add_labors :: State -> State
add_labors State {time=time, ongoing=ongoing, pending=pending} =
    let ready = [c | (c, []) <- pending]
        n = workers_amount - length ongoing
        new_ongoing = ongoing ++ [(c, time + runtime c) | c <- take n ready]
        runninglist = [c | (c, t) <- new_ongoing]
        new_pending = [(c, deps) | (c, deps) <- pending, c `notElem` runninglist]
    in State{time = time, ongoing = new_ongoing, pending = new_pending}


time_flies :: State -> State
time_flies State {time=_, ongoing=ongoing, pending=pending} =
    let time = next_time_point ongoing
        finished = [c | (c, t) <- ongoing, t == time]
        updated_pending = foldr carry_instruction pending finished
        updated_ongoing = [(c, t) | (c, t) <- ongoing, c `notElem` finished]
    in State{time=time, ongoing=updated_ongoing, pending=updated_pending}


sleigh_finished :: State -> Bool
sleigh_finished s = ongoing s == []


solve_B lines = 
    let instructions = parse_instructions lines 
        graph = fill_graph (create_empty_graph instructions) instructions
        list = Map.toAscList graph
        initial = State{time=0, ongoing=[], pending=list}
    in time $ until sleigh_finished (add_labors . time_flies) (add_labors initial)


--------------------------------------------------------

workers_amount = 5
a_time = 61

main = do
    s <- readFile ("07_input.txt")
    let lines = split '\n' s
    putStrLn (show (solve_A lines))
    putStrLn (show (solve_B lines))