module Exercises where


avg :: Double -> Double -> Double
avg x y = (x + y) / 2


median :: Int -> Int -> Int -> Int
median x y z 
    | x < y && x < z = min y z
    | x > y || x > z = min x (max y z)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor False False = False
xor True true = False