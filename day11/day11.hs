import qualified Data.Map as M
import Data.Maybe

type Point = (Int,Int)
type Matrix = M.Map Point Int

charToInt :: Char->Int
charToInt = (\x->read x::Int) . (:"")

convertInputToMap :: String -> Matrix
convertInputToMap input = let input' = map (map charToInt) (lines input) in 
    M.fromList $ concat $ zipWith zip (generatePoints (length input) (length $ head input')) input' 

generatePoints :: Int -> Int -> [[Point]]
generatePoints cl rl = map (flip zip [0..cl] . repeat) [0..rl]

neighbours :: Point -> [Point]
neighbours (x,y) = filter (\(a,b) -> a>= 0 && b>= 0 && a <= 9 && b <= 9) 
    [(x-1, y-1), (x-1, y), (x-1, y+1),(x, y-1), (x, y+1),(x+1, y-1), (x+1, y), (x+1, y+1)] 

increaseAll:: Matrix -> Matrix
increaseAll = fmap (+1)

cleanHighers :: Matrix -> Matrix
cleanHighers = fmap (\x-> if x>9 then 0 else x)
                            
performStep :: (Int, Matrix) -> (Int, Matrix)
performStep (i, mat) = (\(a,b)-> (a+i,b)) (fmap cleanHighers (flashRepeat [] (increaseAll mat)))

flashRepeat :: [Point]->Matrix -> (Int, Matrix)
flashRepeat flashed mat = let (mat', flashed') = flash mat (0,0) flashed in
                            if flashed' == flashed then (length flashed, mat')
                            else flashRepeat flashed' mat' 

flash :: Matrix -> Point -> [Point] -> (Matrix, [Point])
flash mat p@(y,x) alreadyFlashed = case M.lookup p mat of
    Just i ->   let (mat', alreadyFlashed') = if i > 9 && p `notElem ` alreadyFlashed 
                        then (foldl (\a b-> M.insertWith (+) b 1 a) mat (neighbours p), p:alreadyFlashed) 
                        else (mat, alreadyFlashed) 
                    in if x < 9 then flash mat' (y,x+1) alreadyFlashed' 
                        else flash mat' (y+1,0) alreadyFlashed'
    Nothing -> (mat, alreadyFlashed)

step1 ::String-> Int
step1 input = fst . (!! 100) $ iterate performStep (0, convertInputToMap input)

step2 :: String-> Int
step2 input = length $ takeWhile (not . foldr (\x y-> x==0&&y) True. snd) (iterate performStep (0, convertInputToMap input))

main :: IO ()
main = do 
    input <- readFile "example.txt"
    print $  step1 input
    print $  step2 input

