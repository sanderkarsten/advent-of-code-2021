{-# LANGUAGE OverloadedStrings #-}

import Data.List (group, sort)
import qualified Data.Text as T
import Data.Map (fromListWith, toList)
import Data.Maybe

splitStringOn x xs = map T.unpack $ T.splitOn (T.pack x) (T.pack xs)

parseLine :: String -> [String]
parseLine s = let [x,y] = splitStringOn "|" s in tail $ splitStringOn " " y


countSimples :: [String] -> Int
countSimples (x:xs) | length x == 2 || length x == 7 || length x == 4 || length x == 3 = 1 + (countSimples xs)
countSimples (_:xs) = countSimples xs
countSimples [] = 0 

partiallyEquals xs ys = foldr (\ x -> (+) (if x `elem` ys then 1 else 0)) 0 xs

findNumbers (x:xs) m | length x == 2 = findNumbers xs ((1,sort x):m)
findNumbers (x:xs) m | length x == 3 = findNumbers xs ((7,sort x):m)
findNumbers (x:xs) m | length x == 7 = findNumbers xs ((8,sort x):m)
findNumbers (x:xs) m | length x == 4 = findNumbers xs ((4,sort x):m)
findNumbers (x:xs) m | length x == 5 = case lookup 1 m of
                                            Nothing -> findNumbers (xs++[x]) m
                                            Just y ->   if partiallyEquals x y == 2 then findNumbers xs ((3,sort x):m) else 
                                                    case lookup 9 m of
                                                        Nothing -> findNumbers (xs++[x]) m
                                                        Just y -> if partiallyEquals x y == 5 then findNumbers xs ((5,sort x):m) else findNumbers xs ((2,sort x):m) 
findNumbers (x:xs) m | length x == 6 = case lookup 4 m of
                                            Nothing -> findNumbers (xs++[x]) m
                                            Just f -> if partiallyEquals x f == 4 then findNumbers xs ((9,sort x):m) else 
                                                    case lookup 7 m of
                                                            Nothing -> findNumbers (xs++[x]) m
                                                            Just y -> if partiallyEquals y x == 3 then findNumbers xs ((0,sort x):m) else findNumbers xs ((6,sort x):m)
findNumbers (x:xs) m = findNumbers xs m 
findNumbers [] m = m

executeLine s = let [x,y] = map (splitStringOn " ") $ splitStringOn "|" s in let m = findNumbers x [] in map (\x-> fst $ head (filter (\(a,b)-> b == (sort x)) m)) (tail y)

toActualNum (a:b:c:d:_) = a*1000+b*100+c*10+d 

main = do 
    input <- readFile "input.txt"
    print $ sum $ map (toActualNum . executeLine) (lines input)