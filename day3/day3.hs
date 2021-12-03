ct c | c == '0' = -1
ct c | c == '1' = 1

decode =  foldr (zipWith (+) . map ct) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

combine (x:xs) | x < 0 = 0 + 2 * combine xs
combine (x:xs) | x > 0 = 1 + 2 * combine xs
combine [] = 0

main = do 
    input <- readFile "input.txt"
    print $ combine (reverse $ decode (lines input)) * combine (reverse $ map negate (decode (lines input)))