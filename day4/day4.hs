module Day4 where
import Data.List
import Data.Maybe

type StateMat = [[(Int, Bool)]]

strToInt x = read x :: Int

strToState xs = map ((\x->(x,False)) . strToInt) (words xs)

formMats (x:xs) cm | x == "" = cm:formMats xs []
formMats (x:xs) cm = formMats xs (strToState x : cm)
formMats [] cm = [cm]

mark m = map (map (\ (a, b) -> (a, b || a == m)))

cbRow =  foldr ((&&) . snd) True

cbRows = foldr ((||) . cbRow) False

cbCols xs | length (head xs) == 0 = False
cbCols xs = cbRow (map head xs) || cbCols (map tail xs)

checkBingo x = cbCols x || cbRows x 

checkBingos (x:xs) = if checkBingo x then Just x else checkBingos xs
checkBingos [] = Nothing

bingo (x:xs) mats = let tMats = map (mark x) mats in 
                        case checkBingos tMats of
                            Just a -> (a,x)
                            Nothing -> bingo xs tMats

bingoLose (x:xs) mats = let tMats = map (mark x) mats in 
                           let all = map checkBingo tMats in
                               if length (filter (==False) all) == 1 then bingo xs (filter (not . checkBingo) tMats)
                               else bingoLose xs tMats

formGameSequence = map strToInt . wordsWhen (==',')

wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

getUnmarked = filter (not . snd)

unmarkedSum (x:xs) = sum (map fst (getUnmarked x)) + unmarkedSum xs
unmarkedSum [] = 0

main = do 
    input <- readFile "input.txt"
    print $ let (x, y) = (bingo (formGameSequence (head (lines input))) (formMats (tail $ tail $ lines input) [])) in unmarkedSum x * y
    print $ let (x, y) = (bingoLose (formGameSequence (head (lines input))) (formMats (tail $ tail $ lines input) [])) in unmarkedSum x * y