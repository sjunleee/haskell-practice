import Data.Char
import Data.List

-- square
square :: Int -> Int
square x = x * x

-- pyth
pyth :: Int -> Int -> Int
pyth x y = (square x) + (square y)

-- isTriple
isTriple :: Int -> Int -> Int -> Bool
isTriple x y z = (pyth x y) == (square z)

-- isTripleAny
isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny x y z = (isTriple x y z) || (isTriple x z y) || (isTriple y z x)

-- halfEvens
halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

-- inRange
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

-- countPositives
countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

-- capitalised 
capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : [toLower x | x <- xs]

-- title
lowercase :: String -> String
lowercase (x:xs) = toLower x : [toLower x | x <- xs]

capitaliseLong :: String -> String
capitaliseLong word = if (length word >= 4) then (capitalised word) else (lowercase word)

title :: [String] -> [String]
title [] = []
title (word:words) = capitalised word : [capitaliseLong word | word <- words]