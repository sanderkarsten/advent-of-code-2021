{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Map (fromListWith, toList)

type Point = (Int, Int)

strToInt :: String->Int
strToInt x = read x::Int

splitStringOn :: String -> String -> [String]
splitStringOn x xs = map T.unpack $ T.splitOn (T.pack x) (T.pack xs)

intListToPoint :: [Int] -> Point
intListToPoint (x:y:_) = (x,y) 

strToPoints :: String -> [Point]
strToPoints xs = map (intListToPoint . map strToInt . splitStringOn ",") (splitStringOn "->" xs)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

getDangerousOccurences :: [(Point, Int)] -> Int
getDangerousOccurences xs = length (filter ((>=2) . snd) xs)

-- interpolates without diagonals
interpolatePoints :: [Point] -> [Point]
interpolatePoints ((x,y):(x',y'):_) | not (x == x' || y==y') = []
interpolatePoints ((x,y):(x',y'):_) | x < x'  = (x,y):interpolatePoints [(x+1,y),(x',y')]
interpolatePoints ((x,y):(x',y'):_) | y < y'  = (x,y):interpolatePoints [(x,y+1),(x',y')]
interpolatePoints ((x,y):(x',y'):_) | x > x'  = (x,y):interpolatePoints [(x-1,y),(x',y')]
interpolatePoints ((x,y):(x',y'):_) | y > y'  = (x,y):interpolatePoints [(x,y-1),(x',y')]
interpolatePoints (x:xs) = [x]

-- interpolates with diagonals
interpolatePoints' :: [Point] -> [Point]
interpolatePoints' ((x,y):(x',y'):_) | x==x' || y == y' = interpolatePoints [(x,y),(x',y')]
interpolatePoints' ((x,y):(x',y'):_) | x<x' && y<y' = (x,y):interpolatePoints' [(x+1,y+1), (x',y')]
interpolatePoints' ((x,y):(x',y'):_) | x<x' && y>y' = (x,y):interpolatePoints' [(x+1,y-1), (x',y')]
interpolatePoints' ((x,y):(x',y'):_) | x>x' && y<y' = (x,y):interpolatePoints' [(x-1,y+1), (x',y')]
interpolatePoints' ((x,y):(x',y'):_) | x>x' && y>y' = (x,y):interpolatePoints' [(x-1,y-1), (x',y')]
interpolatePoints' (x:xs) = [x]

execute :: String -> ([Point]->[Point]) -> Int
execute input interpolate = getDangerousOccurences $ frequency $ concatMap (interpolate . strToPoints) (lines input)

main = do 
    input <- readFile "real_input.txt"
    print $ execute input interpolatePoints
    print $ execute input interpolatePoints' 