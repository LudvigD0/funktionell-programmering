import Prelude hiding (reverse, take,drop, unzip, zip, maximum)
import Data.List ( (\\) )
import Test.QuickCheck



--Compiling a program

main = print "Hej Test"





--toplevel functions och local defenitions

inc :: Int -> Int
inc x = x + 1

addN :: Int -> [Int] -> [Int]
addN n xs = [inc x | x <- xs]
    where
        inc x = x + n


addM :: Int -> [Int] -> [Int]
addM m xs = let inc x = x + m in [inc x | x <- xs]


append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys



take :: Int -> [a] -> [a]
take n (x:xs) | n > 0 = x : take (n-1) xs
take _ _ = []

drop :: Int -> [a] -> [a]
drop n (z:xs) | n > 0 = drop (n-1) xs
drop _ xs = xs



zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _ _ = []



--unzip skulle han gå igenom men hann aldrig

maximum :: [Int] -> Int
maximum (y:ys) = go y ys 
    where 
        go m [] = m
        go m (x:xs) = go (max m x) xs


reverse :: [a] -> [a]
reverse [] = []
reverse(x: xs) = reverse xs ++ [x]


revFast :: [a] -> [a]
revFast xs = go [] xs
    where
        go acc [] = acc
        go acc (x: xs) = go (x:acc) xs


sort :: Ord a => [a] -> [a]
sort [] = []
sort (p:xs) = sort smaller ++ [p] ++ sort larger
    where
        smaller = [x | x <- xs, x < p]
        larger = [x | x <- xs, x >= p]


prop_idem :: [Int] -> Bool
prop_idem xs = sort xs == sort (sort xs)

prop_permutation :: [Int] -> Bool
prop_permutation xs = null (xs \\ ys) && length xs == length ys
    where
        ys= sort xs


sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- zip xs (tail xs)]

--in the lecture there was another function

