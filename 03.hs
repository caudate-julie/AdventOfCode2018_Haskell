import Utilities

data Rectangle = Rectangle { x1 :: Int
                           , x2 :: Int
                           , y1 :: Int
                           , y2 :: Int }

-- "#4 @ 342,752: 19x17" => (4, 342, 752, 19, 17)
parse_claim :: [Char] -> (Int, Int, Int, Int, Int)
parse_claim s = 
    ( read (slice '#' '@' s),
      read (slice '@' ',' s),
      read (slice ',' ':' s),
      read (slice ':' 'x' s),
      read (slice 'x' ' ' s) )


claim_to_rectangle :: (Int, Int, Int, Int, Int) -> Rectangle
claim_to_rectangle (_, x, y, a, b) = Rectangle { x1 = x
                                               , x2 = x + a
                                               , y1 = y
                                               , y2 = y + b }

--------------------------------------------------------------

pixel_in_area :: (Int, Int) -> Rectangle -> Bool
pixel_in_area (x, y) r = (x1 r <= x) && (x < x2 r) && (y1 r <= y) && (y < y2 r)


-- area -> rectangle in question
rectangles_intersect :: Rectangle -> Rectangle -> Bool
rectangles_intersect p q =
    (x1 p < x2 q) && (x2 p > x1 q) && 
    (y1 p < y2 q) && (y2 p > y1 q)


pixel_has_overlaps :: (Int, Int) -> [Rectangle] -> Bool
pixel_has_overlaps p rs = case filter (pixel_in_area p) rs of
    _:_:_  -> True
    _      -> False


count_overlapped_pixels_in_area :: Rectangle -> [Rectangle] -> Int
count_overlapped_pixels_in_area a rs = 
    let rectangles = filter (rectangles_intersect a) rs
        pixels = [(x, y) | x <- [x1 a .. (x2 a)-1], y <- [y1 a .. (y2 a)-1]] in
    length (filter (flip pixel_has_overlaps rectangles) pixels)


solve_A :: [[Char]] -> Int
solve_A lines = 
    let areas = [Rectangle { x1 = i*32
                           , x2 = i*32 + 32
                           , y1 = j*32
                           , y2 = j*32 + 32 } | i <- [0..31], j <- [0..31]] 
    -- let areas = [Rectangle { x1 = 0, x2 = 1000, y1 = 0, y2 = 1000 }]
        rectangles = (map claim_to_rectangle (map parse_claim lines)) in
    sum [count_overlapped_pixels_in_area a rectangles | a <- areas]

------------------------------------------------------

rectangle_has_overlaps ::  Rectangle -> [Rectangle] -> Bool
rectangle_has_overlaps r rs = any (rectangles_intersect r) rs

find_not_overlapping :: [(Int, Int, Int, Int, Int)] -> [Int]
find_not_overlapping s = 
    let rectangles = map claim_to_rectangle s
        z = zip [id | (id, _, _, _, _) <- s] rectangles in
    [id | (id, r) <- z, not (rectangle_has_overlaps r ((take (id-1) rectangles) ++ (drop id rectangles)))]

solve_B :: [[Char]] -> [Int]
solve_B lines = find_not_overlapping (map parse_claim lines)

-----------------------------------------------------

main = do
    s <- readFile "03_input.txt"
    let lines = split '\n' s
    -- putStrLn (show (count_pixel (3, 2) s))
    putStrLn (show (solve_A lines))
    putStrLn (show (solve_B lines))

