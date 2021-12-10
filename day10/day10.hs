import qualified Data.Map as M
import Data.Maybe
import Data.List (sort)
import Data.Either

isOpen c = c `elem` ['{','[','(','<']

reverseBrac c = fromMaybe c (lookup c [(']', '['),('}', '{'), ('>', '<'), (')', '(')])
charToScore c = fromMaybe 0 (lookup c [ (']', 57), ('}', 1197), (')', 3), ('>', 25137), 
                                        ('(', 1), ('[', 2), ('{', 3), ('<', 4)])

parseLine stack (c:cs) | isOpen c = parseLine (c:stack) cs
parseLine (s:stack) (c:cs)  | reverseBrac c == s = parseLine stack cs 
parseLine (s:stack) (c:cs) = Left c
parseLine cs [] = Right cs
parseLine _ _ = error "Invalid parse"

countScore :: Int -> String -> Int
countScore = foldl (\ i c -> 5 * i + charToScore c)

part1 input = sum $ map charToScore $ lefts $ map (parseLine "") input
part2 input = let scores = map (countScore 0) $ rights $ map (parseLine "") input in sort scores !! (length scores `div` 2)
main = do 
    input <- readFile "example.txt"
    print $ part1 (lines input)
    print $ part2 (lines input)