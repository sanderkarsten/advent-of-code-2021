parse ('f':'o':'r':'w':'a':'r':'d':' ':xs) = (strToInt xs,0)
parse ('d':'o':'w':'n':' ':xs) = (0,strToInt xs)
parse ('u':'p':' ':xs) = (0,-strToInt xs)

strToInt :: String->Int
strToInt x = read x::Int

dive [] (h,d) = h*d
dive (x:xs) (h,d) = dive xs $ (\(a,b)-> (a+h, b+d)) $ parse x

diveAim [] (h,d,_) = h*d
diveAim (x:xs) (h,d,a) = diveAim xs $ (\(x,y)-> (x+h, a*x+d,a+y)) $  parse x

main = do 
    input <- readFile "input1.txt"
    print (dive (lines input) (0,0))
    print (diveAim (lines input) (0,0,0))    