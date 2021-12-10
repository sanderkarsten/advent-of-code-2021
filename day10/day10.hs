import qualified Data.Map as M
import Data.Maybe
import Data.List (sort)

isOpen c = c `elem` ['{','[','(','<']
isClose c = c `elem` ['}', ']',')', '>']

reverseBrac ']' = '['
reverseBrac '}' = '{'
reverseBrac '>' = '<'
reverseBrac ')' = '('
reverseBrac c = c

charToScore ']' = 57
charToScore '}' = 1197
charToScore ')' = 3
charToScore '>' = 25137
charToScore '[' = 2
charToScore '{' = 3
charToScore '(' = 1
charToScore '<' = 4
charToScore _ = 0

parseLine :: String -> String -> Maybe Char
parseLine stack (c:cs) | isOpen c = parseLine (c:stack) cs
parseLine (s:stack) (c:cs)  | reverseBrac c == s = parseLine stack cs 
parseLine (s:stack) (c:cs) = Just c
parseLine (c:cs) [] = Nothing
parseLine [] (s:stack) = Nothing 
parseLine _ _ = Nothing 

parseLine' :: String -> String -> Maybe String
parseLine' stack (c:cs) | isOpen c = parseLine' (c:stack) cs
parseLine' (s:stack) (c:cs)  | reverseBrac c == s = parseLine' stack cs 
parseLine' (s:stack) (c:cs) = Nothing
parseLine' cs [] = Just cs
parseLine' [] stack = Nothing

countScore Nothing = 0
countScore (Just x) = charToScore x

countScore' :: Int -> String -> Int
countScore'= foldl (\ i c -> 5 * i + charToScore c)

main = do 
    input <- readFile "input.txt"
    print $ sum $ map (countScore . parseLine "") (lines input)
    print $ let scores = map (countScore' 0) (mapMaybe (parseLine' "") (lines input)) in
                sort scores !! (length scores `div` 2)
