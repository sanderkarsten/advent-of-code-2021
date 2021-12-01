module Day1_2 where
import Data.List

count [] = 0
count(a:b:c:d:xs) | a+b+c<b+c+d = 1 + count (b:c:d:xs)
count (x:xs) = count xs

strToIntList x = map (\x -> read x::Int) (lines x)

main :: IO()
main = do 
    input <- readFile "input_2.txt"
    putStr . show $ count (strToIntList input)