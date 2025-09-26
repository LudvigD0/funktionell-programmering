

import Prelude hiding (foldr, and, or)

{- concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss -}




foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op b []      = b  
foldr op b (x: xs) = x `op` foldr op b xs


--sum' xs = foldr (\x y -> x + y) 0 xs
sum' = foldr (+) 0
prod' = foldr (*) 1


and, or :: [Bool] -> Bool
and bs = foldr (&&) True bs
or bs = foldr (||) False bs


reveeeee [] = []
reveeeee xs = foldl (\acc x -> x:acc) [] xs



o :: (b -> c) -> (a -> b) -> a -> c
f `o` g = \x -> f (g x)




rev' :: [a] -> [a]
rev' = foldl (\acc x -> x:acc) []

    