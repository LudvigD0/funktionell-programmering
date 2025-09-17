


module Lecture3A where

import Data.List (transpose)
import Test.QuickCheck hiding ((><))


mul, mul' :: Int -> Int -> Int
mul x y = x * y
mul' x y = (*) x y


(><) :: Bool -> Bool -> Bool
True >< False = True
False >< True = True

_ >< _ = False


infixr 4 ><


(|>) :: (a, a) -> [a] -> [a]
(x, y) |> ys = x : y : ys

infixr 5 |>


--lamda expressions - funktion utan namn
sq :: Int -> Int
sq = \x -> x * x 
--samma sak:
--sq x = x * x

answer :: Int
answer = 42

list :: [Int]
list = [1,2,4,answer]

b:: Bool
b = False

addl = \x y -> x + y



add2, add1, add0 :: Int -> Int -> Int
add2 x y = x + y
add1 x = \y -> x + y
add0   = \x -> ( \y -> x + y)



f :: Int -> (Int -> (Int -> Int))
f = \x -> (\y -> (\z -> x + y +z))


{-
add0 2 3

(add0 2) 3

((\x -> (\y -> x + y)) 2) 

(\y -> 2 + y) 3

2 + 3

5
-}


inc :: Int -> Int
inc = add0 1



haha :: [String]
haha = [takeTwo xs | xs <- ["hasell", "hallo", "have"]]
    where
        takeTwo = take 2



withIndex :: [a] -> [(Int, a)]
withIndex xs = zip [0 ..] xs


rev :: [a] -> [a]
rev = go []
    where 
        go acc [] = acc
        go acc (x: xs) = go (x:acc) xs



--fråga chat, varför kan man ta bort xs:
{-
rev :: [a] -> [a]
rev xs = go [] xs
    where 
        go acc [] = acc
        go acc (x: xs) = go (x:acc) xs

fråga också vad go [] retunerar pga att funktioner är uppbyggda av (go [] xs)
-}



--triple :: [Int] -> [Int]
--triple xs = let f =(3 *) in [f x | ]