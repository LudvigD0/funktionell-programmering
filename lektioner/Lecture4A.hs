import Prelude hiding (filter)

{- 

--Göra funktioner mer generella
--Lägga till extra parameters till funktionen

--eta reduction





countSuit :: Suit -> Hand -> Int
countSuit s hand = length [suit | Card _ suit <- hand, suit == s]

countClubs1, countSpades1 :: Hand -> Int
countClubs1 hand = countSuit Clubs hand
countSpades1 hand = countSuit Spades hand

--samma sak
countClubs1 = countSuit Clubs
countSpades1 = countSuit Spades
 -}

{- doubles :: [Int] -> [Int]
doubles [] = []
dobules (x:xs) = 2*x :  doubles xs



squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = x^2 : squares xs

map f []     = []
map f (x:xs) = f x : map f xs


doubles1 = map (2*)
squares1 = map (^2)
 -}


evens :: [Int] -> [Int]
evens [] = []
evens(x:xs)
    | even x    = x : evens xs
    | otherwise = evens xs

largerThan :: [Int] -> [Int]
largerThan [] = []
largerThan (x:xs)
    | x > 7 = x : largerThan xs
    | otherwise = largerThan xs

 
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []    
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs


