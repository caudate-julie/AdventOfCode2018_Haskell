-- Sum from file


findChar s c n = if (length s == 0 || s!!0 == c) 
    then n 
    else findChar (tail s) c (n+1)


getSum s = if (length s == 0) 
    then 0
    else 
        if s!!0 == '+' 
            then getSum (tail s)
            else
                let n = findChar s '\n' 0 
                in read (take n s) + getSum (drop (n+1) s)


main = do
    s <- readFile "01_input.txt"
--    let s = "+12\n-13\n+14\n"
    putStrLn (show (getSum s))               -- show ~ repr


