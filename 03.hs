import Utilities


-- "#4 @ 342,752: 19x17" => (342, 752, 19, 17)
parse_claim :: [Char] -> (Int, Int, Int, Int, Int)
parse_claim s = 
    ( read (slice '#' '@' s),
      read (slice '@' ',' s),
      read (slice ',' ':' s),
      read (slice ':' 'x' s),
      read (slice 'x' ' ' s) )


parse_claims :: [[Char]] -> [(Int, Int, Int, Int, Int)]
parse_claims lines = map parse_claim lines


claim_to_rectangle :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int)
claim_to_rectangle (_, x, y, a, b) = (x, x+a, y, y+b)


--------------------------------------------------------------

pixel_in_area :: (Int, Int) -> (Int, Int, Int, Int) -> Bool
pixel_in_area (x, y) (x1, x2, y1, y2) = (x1 <= x) && (x < x2) && (y1 <= y) && (y < y2)


-- area -> rectangle in question
rectangle_in_area :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
rectangle_in_area (ax1, ax2, ay1, ay2) (x1, x2, y1, y2) =
    (ax1 < x2) && (ax2 > x1) && 
    (ay1 < y2) && (ay2 > y1)


pixel_overlap :: (Int, Int) -> [(Int, Int, Int, Int)]  -> Bool
pixel_overlap p as = case filter (pixel_in_area p) as of
    _:_:_  -> True
    _      -> False


count_area :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)] -> Int
count_area a rs = 
    let rectangles = [r | r <- rs, rectangle_in_area a r]
        (x1, x2, y1, y2) = a
        pixels = [(x, y) | x <- [x1..x2-1], y <- [y1..y2-1]] in
    length (filter (flip pixel_overlap rectangles) pixels)


solve_A :: [[Char]] -> Int
solve_A s = 
    -- let areas = [(i*32, i*32 + 32, j*32, j*32 + 32) | i <- [0..31], j <- [0..31]] 
    let areas = [(0, 1000, 0, 1000)]
        rectangles = (map claim_to_rectangle (parse_claims s)) in
    sum [count_area a rectangles | a <- areas]

------------------------------------------------------

-- rectangle has no overlaps
overlapping ::  (Int, Int, Int, Int) -> [(Int, Int, Int, Int)] -> Bool
overlapping r rs = any (rectangle_in_area r) rs

find_not_overlapping :: [(Int, Int, Int, Int, Int)] -> [Int]
find_not_overlapping s = 
    let rectangles = map claim_to_rectangle s
        z = zip [id | (id, _, _, _, _) <- s] rectangles in
    [id | (id, r) <- z, not (overlapping r ((take (id-1) rectangles) ++ (drop id rectangles)))]

solve_B :: [[Char]] -> [Int]
solve_B s =  find_not_overlapping (parse_claims s)


-----------------------------------------------------



main = do
    s <- readFile "03_input.txt"
    let lines = split '\n' s
    -- putStrLn (show (count_pixel (3, 2) s))
    -- putStrLn (show (solve_A lines))
    putStrLn (show (solve_B lines))

