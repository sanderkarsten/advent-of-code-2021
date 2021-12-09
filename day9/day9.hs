-- {-# LANGUAGE OverloadedStrings #-}
import Data.List (group, sort)
import qualified Data.Text as T
import Data.Map (fromListWith, toList)
import Data.Maybe

strToInt :: String->Int
strToInt x = read x::Int

charToInt = strToInt . (:"")

parseInput xs = map (map charToInt) (lines xs)

dataAt _ [] = Nothing
dataAt y x | y < 0 = Nothing
dataAt y (x:xs)  | y <= 0 = Just x
                 | otherwise = dataAt (y-1) xs

iterateOverMat row column mat | not (null mat) && length (head mat) == column = iterateOverMat (row+1) 0 mat
iterateOverMat row column mat | length mat == row = 0
iterateOverMat row column mat = let currentRow = mat !! row in let currentVal = currentRow !! column in 
    if  maybe True (\ nextRow -> currentVal < (nextRow !! column)) (dataAt (row + 1) mat) &&
        maybe True (\prevRow -> currentVal < (prevRow !! column)) (dataAt (row - 1) mat) &&
        maybe True (currentVal <) (dataAt (column+1) currentRow) &&
        maybe True (currentVal <) (dataAt (column-1) currentRow)
      then 1+currentVal + iterateOverMat row (column+1) mat else iterateOverMat row (column+1) mat

getElem m (x, y) | 0 <= y && y < length m && 0 <= x && x < (length. head) m = m !! y !! x
getElem _ _ = 9

neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isPointMin (x, y) m = all ((> getElem m (x,y)) . getElem m) (neighbours (x,y))

findMinPoints (x, y) m | x < (length . head) m && isPointMin (x,y) m = (x,y) : findMinPoints (x + 1, y) m
findMinPoints (x, y) m | x < (length . head) m = findMinPoints (x + 1, y) m
findMinPoints (x, y) m | y < length m = findMinPoints (0, y + 1) m
findMinPoints _ _  = []

findBasin m s p | getElem m p < 9 && notElem p s = foldl (findBasin m) (p:s) (neighbours p)
findBasin _ s _ = s

findBasins m x =  product $ drop (length x - 3) (sort (map ((sum . map length) . findBasin m []) x))

main = do 
    input <- readFile "input.txt"
    print $ findBasins (parseInput input) (findMinPoints (0,0) (parseInput input))