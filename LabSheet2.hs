-- safetail
safetail :: [a] -> [a]
safetail xs | null xs = []
            | otherwise = tail xs

-- factorial using recursion
factorialRecursive :: Int -> Int
factorialRecursive 0 = 1
factorialRecursive n = n * factorialRecursive (n-1)

-- quicksort using recursion
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x] 
               ++ [x] ++ 
               qsort [b | b <- xs, b > x]

-- define insert function using recursion
insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs)
  | n <= x = n : x : xs
  | otherwise = x : insert n xs