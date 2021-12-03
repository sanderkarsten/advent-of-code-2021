toInt x = read x :: Int

bitToChange c | c == '0' = -1
bitToChange c | c == '1' = 1

decode =  foldr (zipWith (+) . map bitToChange) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

combine (x:xs) | x < 0 = 0 + 2 * combine xs
combine (x:xs) | x > 0 = 1 + 2 * combine xs
combine [] = 0

firstRowTotal (x:xs) = bitToChange (head x) + firstRowTotal xs
firstRowTotal _ = 0

o [] = []
o (x:xs) | null x = []
o xs | length xs == 1 = map (toInt . (:""))  (head xs)
o xs | firstRowTotal xs < 0 = let x = filter (\a-> bitToChange (head a) == -1) xs in 0: o (map tail x)
o xs | firstRowTotal xs >= 0 = let x = filter (\a-> bitToChange (head a) == 1) xs in 1: o (map tail x)

c [] = []
c xs | length xs == 1 = map (toInt . (:""))  (head xs)
c xs | firstRowTotal xs < 0 = let x = filter (\a-> bitToChange (head a) == 1) xs in 1: c (map tail x)
c xs | firstRowTotal xs >= 0 = let x = filter (\a-> bitToChange (head a) == -1) xs in 0: c (map tail x)

main = do 
    input <- readFile "input.txt"
    print $ combine (reverse $ decode (lines input)) * combine (reverse $ map negate (decode (lines input)))
    print $ o (lines input)
    print $ c (lines input)