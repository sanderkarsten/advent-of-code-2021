{-# LANGUAGE OverloadedStrings #-}

import Data.List (group, sort)
import qualified Data.Text as T
import Data.Map (fromListWith, toList)

strToInt x = read x::Int

splitStringOn x xs = map T.unpack $ T.splitOn (T.pack x) (T.pack xs)

parseInput = map strToInt . splitStringOn ","

sumDegrade a | a > 0= a + sumDegrade (a-1)
sumDegrade a = 0 

countDiffSum  i xs = sum $ map (\a-> abs (a-i)) xs

generateStepLengths xs = map sumDegrade [0..(maximum xs)]

countDiffSum' s i = sum . map (\a-> (!! abs (a - i)) s)

findMinSum xs i | i == maximum xs = i
findMinSum xs i = let y = countDiffSum i xs in if y <= countDiffSum (i+1) xs then y else findMinSum xs (i+1)

findMinSum' s xs i | i == maximum xs = i
findMinSum' s xs i = let y = countDiffSum' s i xs in if y <= countDiffSum' s (i+1) xs then y else findMinSum' s xs (i+1)

main = do 
    input <- readFile "input.txt"
    print $ let i = parseInput input in findMinSum i 0
    print $ let i = parseInput input in findMinSum' (generateStepLengths i) i 0