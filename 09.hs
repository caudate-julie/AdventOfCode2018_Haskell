{-# LANGUAGE BangPatterns #-}


-- 6 : 1 2 (3) <|> 4 5 -> 1 2 3 4 (6) <|> 5
-- 6 : 1 2 (3) <|> -> <|> 1 2 3 -> 1 (6) <|> 2 3
move_left :: Int -> ([a], [a]) -> ([a], [a])
move_left 0 (left, right) = (left, right)
move_left n (l:left, right) = move_left (n-1)(left, l:right)
move_left n ([], right) = move_left n (reverse right, [])


move_right :: Int -> ([a], [a]) -> ([a], [a])
move_right 0 (left, right) = (left, right)
move_right n (left, r:right) = move_right (n-1) (r:left, right)
move_right n (left, []) = move_right n ([], reverse left)


insert :: a -> ([a], [a]) -> ([a], [a])
insert x (left, right) = (x:left, right)


delete :: ([a], [a]) -> (a, ([a], [a]))
delete (x:left, right) = (x, (left, right))
delete ([], right) = delete (reverse right, [])



-- current marble -> max marble -> scores -> (left, right) -> final score
play :: Int -> Int -> [Int] -> ([Int], [Int]) -> Int
play n maxx scores circle | n > maxx         = maximum scores
                          | n `mod` 23 == 0  =
                            let
                                (start, p:end) = splitAt (n `mod` length scores) scores
                                circle' = move_left 7 circle
                                (s, circle'') = delete circle'
                                circle''' = move_right 1 circle''
                            in play (n+1) maxx (start ++ [p + s + n] ++ end) circle'''
                          | otherwise        = 
                            let circle' = move_right 1 circle
                                circle'' = insert n circle'
                            in play (n+1) maxx scores circle''
        

solve_A players marbles = 
    let scores = replicate players 0
    in play 1 marbles scores ([0], [])


main = do
    -- examples
    -- putStrLn $ show $ solve_A 9 25        -- 32
    -- putStrLn $ show $ solve_A 10 1618     -- 8317
    -- putStrLn $ show $ solve_A 13 7999     -- 146373
    -- putStrLn $ show $ solve_A 17 1104     -- 2764
    -- putStrLn $ show $ solve_A 21 6111     -- 54718
    -- putStrLn $ show $ solve_A 30 5807     --  37305
    -- real task
    putStrLn $ show $ solve_A 464 7173000
