import Utilities
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- slice '?' '!' "ab?def!g" = "def"
-- slice '?' '!' "ab?defg" = "defg"
slice :: (Eq a) => a -> a -> [a] -> [a]
slice x y s = 
    let ix = findChar x s
        s1 = drop (ix+1) s
        iy = findChar y s1 in
    take iy s1


-- "#4 @ 342,752: 19x17" => (342, 752, 19, 17)
parse_claim :: [Char] -> (Int, Int, Int, Int)
parse_claim s = 
    ( read (slice '@' ',' s),
      read (slice ',' ':' s),
      read (slice ':' 'x' s),
      read (slice 'x' ' ' s) )


parse_claims :: [[Char]] -> [(Int, Int, Int, Int)]
parse_claims lines = map parse_claim lines


claims_to_rectangles :: [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
claims_to_rectangles xs = [(x, x+a, y, y+b) | (x, y, a, b) <- xs]


--------------------------------------------------------------

pixel_in_area :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
pixel_in_area (x1, x2, y1, y2) (x, y) = (x1 <= x) && (x < x2) && (y1 <= y) && (y < y2)


-- rectangle is in area if it overlaps
rectangle_in_area :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
rectangle_in_area (ax1, ax2, ay1, ay2) (x1, x2, y1, y2) =
    (ax1 < x2) && (ax2 > x1) && 
    (ay1 < y2) && (ay2 > y1)


count_pixel :: (Int, Int) -> [(Int, Int, Int, Int)]  -> Int
count_pixel (x, y) [] = 0
count_pixel (x, y) (a:as) = case count_pixel (x, y) as of
    2   -> 2
    n   -> n + (if pixel_in_area a (x, y) then 1 else 0)


count_area :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]  -> Int
count_area a rs = 
    let rectangles = [r | r <- rs, rectangle_in_area a r]
        (x1, x2, y1, y2) = a
        pixels = [(x, y) | x <- [x1..x2-1], y <- [y1..y2-1]] in
    sum[1 | p <- pixels, count_pixel p rectangles == 2]


solve_A :: [[Char]] -> Int
solve_A s = 
    let areas = [(i*32, i*32 + 32, j*32, j*32 + 32) | i <- [0..31], j <- [0..31]] 
        rectangles = (claims_to_rectangles . parse_claims) s in
    sum[count_area a rectangles | a <- areas]

------------------------------------------------------


solve_B :: [[Char]] -> [Char]
solve_B s = error "TODO"

-----------------------------------------------------



main = do
    s <- readFile "03_input.txt"
    let lines = split '\n' s
    -- let lines = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]
    -- let s = (claims_to_rectangles . parse_claims) lines
    -- putStrLn (show (count_pixel (3, 2) s))
    putStrLn (show (solve_A lines))

