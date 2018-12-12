import Data.Char
import Utilities

collapse :: Char -> Char -> Bool
collapse x y = Data.Char.toLower x == Data.Char.toLower y && x /= y


polymer :: [Char] -> [Char]
polymer (x:xs) =
    case polymer xs of
        p:ps -> if collapse x p 
                then ps
                else x:p:ps
        []   -> [x]
polymer [] = []

-- polymer_improved :: Char -> [Char] -> [Char]
-- polymer_improved c (x:xs) = 
--     if Data.Char.toLower x == c 
--     then polymer_improved c xs
--     else
--         case polymer_improved c xs of
--             p:ps -> if collapse x p then ps
--                     else x:p:ps
--             []   -> [x]
-- polymer_improved c [] = []

solve_A s = length (polymer s)


solve_B :: [Char] -> Int
solve_B s = 
    minimum [length (polymer (filter (removed c) s)) | c <- ['a'..'z']]
    where
        removed c x = Data.Char.toLower x /= c


main = do
    s <- readFile "05_input.txt"
    let [line] = split '\n' s
    putStrLn (show (solve_A line))
    putStrLn (show (solve_B line))
