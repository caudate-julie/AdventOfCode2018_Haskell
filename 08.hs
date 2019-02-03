import Utilities


data Node = Node [Node] [Int]


iter_apply :: Int -> (a -> (b, a)) -> a -> ([b], a)
iter_apply 0 f x = ([], x)
iter_apply n f x = 
    let (cur_b, cur_a) = f x
        (next_bs, next_a) = iter_apply (n - 1) f cur_a
    in ([cur_b] ++ next_bs, next_a)


parse_license :: [Int] -> (Node, [Int])
parse_license (degree:meta:ndata) = 
    let (children, nextdata) = iter_apply degree parse_license ndata
    in (Node children (take meta nextdata), (drop meta nextdata))


sum_meta :: Node -> Int
sum_meta (Node [] meta) = sum meta
sum_meta (Node children meta) = sum meta + sum (map sum_meta children)


solve_A license = sum_meta license

--------------------------------------------------------

node_value :: Node -> Int
node_value (Node [] meta) = sum meta
node_value (Node children meta) = 
    let values = map node_value children
    in sum([values!!(i-1) | i <- meta, i > 0, i <= length children])

solve_B license = node_value license

--------------------------------------------------------

main = do
    s <- readFile("08_input.txt")
    -- let s = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    let digits = map read (split ' ' s)
    let (license, _) = parse_license digits
    putStrLn (show (solve_A license))
    putStrLn (show (solve_B license))