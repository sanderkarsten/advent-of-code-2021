{-# LANGUAGE OverloadedStrings #-}

import Data.List (group, sort)
import qualified Data.Text as T
import Data.Map (fromListWith, toList)
import Data.Maybe
import Distribution.Simple.Utils (xargs)

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

main = do 
    input <- readFile "input.txt"
    print $ iterateOverMat 0 0 (parseInput input)