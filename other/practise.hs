


sumEven :: [Int] -> Int
sumEven list = go list 
    where
        go [] = 0
        go (x, xs) | x + go xs

