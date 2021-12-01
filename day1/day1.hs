module Day1 where
import Data.List

count(x:y:xs) | x < y = 1 + count (y:xs)
count (_:xs) = count xs
count _ = 0

countSum(a:b:c:d:xs) | a+b+c<b+c+d = 1 + countSum (b:c:d:xs)
countSum (_:xs) = countSum xs
countSum _ = 0

strToIntList = map (\x -> read x::Int) . lines

main :: IO()
main = do 
    input <- readFile "input.txt"
    input2 <- readFile "input_2.txt"
    putStrLn . show . count . strToIntList $ input
    putStrLn . show . countSum . strToIntList $ input2
        