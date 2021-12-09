import Data.List (sort)

strToInt :: String->Int
strToInt x = read x::Int

charToInt = strToInt . (:"")

parseInput xs = map (map charToInt) (lines xs)

getElem :: [[Int]] -> (Int,Int)-> Int
getElem mat (row,col) | 0 <= col && col < length mat && 0 <= row && row < (length. head) mat = mat !! col !! row
getElem _ _ = 9

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (row, col) = [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]

findMinPoints :: Int->Int -> [[Int]] -> [(Int, Int)]
findMinPoints row col mat | row < (length . head) mat && all ((> getElem mat (row, col)) . getElem mat) (neighbours (row, col)) = (row, col) : findMinPoints (row + 1) col mat
findMinPoints row col mat | row < (length . head) mat = findMinPoints (row + 1) col mat
findMinPoints row col mat | col < length mat = findMinPoints 0 (col + 1) mat
findMinPoints _ _ _  = []

-- Part 1:
getRiskLevel :: [[Int]] -> Int
getRiskLevel mat = sum (map ((+1) . getElem mat) (findMinPoints 0 0 mat))

-- Part 2:
findBasin :: [[Int]] -> [(Int, Int)] -> (Int,Int) -> [(Int, Int)]
findBasin mat seen point | getElem mat point < 9 && notElem point seen = foldl (findBasin mat) (point:seen) (neighbours point)
findBasin _ seen _ = seen

findBasins :: [[Int]] -> [(Int, Int)] -> Int
findBasins mat min =  product $ drop (length min - 3) (sort (map ((sum . map length) . findBasin mat []) min))

main = do 
    input <- readFile "input.txt"
    print $ getRiskLevel (parseInput input)
    print $ findBasins (parseInput input) (findMinPoints 0 0 (parseInput input))