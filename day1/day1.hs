module Day1 where
import Data.List

count [] = 0
count(x:y:xs) | x < y = 1 + count (y:xs)
count (x:xs) = count xs

strToIntList x = map (\x -> read x::Int) (lines x)

main :: IO()
main = do 
    input <- readFile "input.txt"
    putStr . show $ count (strToIntList input)