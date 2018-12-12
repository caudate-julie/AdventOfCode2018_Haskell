import Utilities
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data State = Asleep Int | Awaken Int deriving Show
data Note = GShift Int | GSleeps | GWakes deriving Show    -- TODO: understand


instance Eq State where
    (Asleep x) == (Asleep y) = x == y
    (Asleep x) == (Awaken y) = x == y
    (Awaken x) == (Asleep y) = x == y
    (Awaken x) == (Awaken y) = x == y

instance Ord State where
    (Asleep x) `compare` (Asleep y) = x `compare` y
    (Asleep x) `compare` (Awaken y) = x `compare` y
    (Awaken x) `compare` (Asleep y) = x `compare` y
    (Awaken x) `compare` (Awaken y) = x `compare` y


parse_note :: [Char] -> (Note, Int, Int, Int, Int)
parse_note x = 
    let month = read (take 2 (drop 6 x))
        day = read (take 2 (drop 9 x))
        minute = read (take 2 (drop 15 x))
        hour = read (take 2 (drop 12 x))
        note = case x!!19 of
            'G'  -> GShift (read (slice '#' ' ' x))
            'w'  -> GWakes
            'f'  -> GSleeps
    in (note, month, day, hour, minute)


-- returns (Guard, State)
get_time_intervals :: [(Note, Int, Int, Int, Int)] -> Int -> [(Int, State)]
get_time_intervals [] _ = []
get_time_intervals ((GShift g, _, _, hour, minute):xs) _ = 
    get_time_intervals xs g

get_time_intervals ((GWakes, _, _, _, minute):xs) g =
    [(g, Awaken minute)] ++ get_time_intervals xs g

get_time_intervals ((GSleeps, _, _, _, minute):xs) g =
    [(g, Asleep minute)] ++ get_time_intervals xs g


-- unsorted unparsed wall notes to {Guard, [State]}
build_map :: [[Char]] -> Map Int [State]
build_map lines = 
    let intervals = get_time_intervals (map parse_note (sort lines)) 0 in
        Map.fromListWith (++) [(x, [s]) | (x, s) <- intervals]


-- list of sleep-wake states to total sleeptime
sleeptime :: [State] -> Int
sleeptime [] = 0
sleeptime ((Awaken t):ts) = t + sleeptime ts
sleeptime ((Asleep t):ts) = -t + sleeptime ts


maxindex :: (Ord b) => [(a, b)] -> (a, b)
maxindex [] = error "Empty guard"
maxindex [x] = x
maxindex ((g, t):xs) = let (g', t') = maxindex xs in
    if t > t' then (g, t) else (g', t')


sleepest_guard :: Map Int [State] -> Int
sleepest_guard m = 
    let mt = Map.toList (Map.map sleeptime m)
        (g, t) = maxindex mt
    in g


state_signum (Awaken _) = -1
state_signum (Asleep _) = 1

state_minute (Awaken m) = m
state_minute (Asleep m) = m


-- states of one guard to [(minute, repeats)]
minute_list :: [State] -> [(Int, Int)]
minute_list [Asleep m] = [(m, 1)]
minute_list (s:xs) = 
    let m = state_minute s
        r = state_signum s
        (m', r'):ms = minute_list xs in

    if (m == m') 
    then [(m', r'+ r)] ++ ms
    else [(m, r' + r)] ++ [(m', r')] ++ ms


solve_A :: [[Char]] -> Int
solve_A s = 
    let m = build_map s
        guard = sleepest_guard m
        ml = minute_list (reverse (sort (m Map.! guard)))
        (x, y) = maxindex ml
    in x * guard

--------------------------------------------------------

-- solve_B :: [[Char]] -> Int
solve_B s = 
    let m = build_map s
        gm = Map.map (maxindex . minute_list . reverse . sort) m
        gml = Map.toList gm
        gpairs = [(g*m, t) | (g, (m, t)) <- gml]
        (x, y) = maxindex gpairs
    in x

main = do
    s <- readFile "04_input.txt"
    let lines = split '\n' s
    putStrLn (show (solve_A lines))
    putStrLn (show (solve_B lines))
