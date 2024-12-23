import System.IO ()

element :: [[Int]] -> (Int, Int) -> Int
element sudoku (x, y) = (sudoku !! x) !! y

findNextEmpty :: [[Int]] -> Maybe (Int, Int)
findNextEmpty sudoku =
    let emptyCells = [(x, y) | x <- [0..8], y <- [0..8], element sudoku (x, y) == 0]
    in if null emptyCells then Nothing else Just (head emptyCells)

removeZeros :: [Int] -> [Int]
removeZeros [] = []
removeZeros (x:xs) =
    if x == 0
    then removeZeros xs
    else x : removeZeros xs


rowTakenNumbers :: [[Int]] -> Int -> [Int]
rowTakenNumbers sudoku r = removeZeros $ sudoku !! r

getColumn :: [[Int]] -> Int -> [Int]
getColumn sudoku c = foldl f [] [0..8]
                where f acc a = acc ++ [element sudoku (a, c)]


columnTakenNumbers :: [[Int]] -> Int -> [Int]
columnTakenNumbers sudoku c = removeZeros $ getColumn sudoku c

getSubArray :: [[Int]] -> (Int, Int) -> [Int]
getSubArray sudoku (x, y) = removeZeros [element sudoku (a, b) | a <- [x .. x + 2], b <- [y .. y + 2]]


subArrayTakenNumbers :: [[Int]] -> (Int, Int) -> [Int]
subArrayTakenNumbers sudoku (r, c)
    | r <= 2 && c <= 2 = getSubArray sudoku (0, 0)
    | r <= 2 && c <= 5 = getSubArray sudoku (0, 3)
    | r <= 2           = getSubArray sudoku (0, 6)
    | r <= 5 && c <= 2 = getSubArray sudoku (3, 0)
    | r <= 5 && c <= 5 = getSubArray sudoku (3, 3)
    | r <= 5           = getSubArray sudoku (3, 6)
    | c <= 2           = getSubArray sudoku (6, 0)
    | c <= 5           = getSubArray sudoku (6, 3)
    | otherwise        = getSubArray sudoku (6, 6)


allTakenNumbers :: [[Int]] -> (Int, Int) -> [Int]
allTakenNumbers sudoku (r, c) = rowTakenNumbers sudoku r ++ columnTakenNumbers sudoku c ++ subArrayTakenNumbers sudoku (r, c)


posibleNumbers :: [[Int]] -> (Int, Int) -> [Int]
posibleNumbers sudoku (r, c) = [n | n <- [1..9], n `notElem` allTakenNumbers sudoku (r, c)]


solve :: [[Int]] -> (Bool, [[Int]])
solve sudoku = 
    case findNextEmpty sudoku of
        Nothing -> (True, sudoku) 
        Just (x, y) -> tryCandidates sudoku (x, y) (posibleNumbers sudoku (x, y))


tryCandidates :: [[Int]] -> (Int, Int) -> [Int] -> (Bool, [[Int]])
tryCandidates sudoku _ [] = (False, sudoku) 
tryCandidates sudoku (x, y) (n:ns) = if fst (solve (updateSudoku sudoku (x, y) n)) then 
                                        (True, snd (solve (updateSudoku sudoku (x, y) n)))
                                    else 
                                        tryCandidates sudoku (x, y) ns



updateSudoku :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
updateSudoku sudoku (x, y) n =
    take x sudoku ++ [take y (sudoku !! x) ++ [n] ++ drop (y + 1) (sudoku !! x)] ++ drop (x + 1) sudoku


main = do
        contents <- readFile "sudoku.txt"
        let sudoku = map (map read . words) (lines contents) :: [[Int]] 
        let (solved, result) = solve sudoku
        if solved 
            then mapM_ (putStrLn . unwords . map show) result
            else putStrLn "No solution found"