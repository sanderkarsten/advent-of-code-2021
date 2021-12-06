{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06 where

import Data.List (group, sort)
import qualified Data.Text as T
import Data.Map (fromListWith, toList)

strToInt x = read x::Int

splitStringOn x xs = map T.unpack $ T.splitOn (T.pack x) (T.pack xs)

parseInput = map strToInt . splitStringOn ","

shift (x : xs) = let (y, [a, b]) = splitAt 6 xs in y ++ [a + x] ++ [b, x]

initState = map (\x->length x-1) . group . sort . (++ [0 .. 8])

main = do 
    input <- readFile "input.txt"
    print $ sum ((!! 80) (iterate shift (initState (parseInput input))))
    print $ sum ((!! 256) (iterate shift (initState (parseInput input))))