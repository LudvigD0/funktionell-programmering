import Prelude hiding (Maybe, Just, Nothing)


data List a = Nil | Cons a (List a) deriving Show


xs :: List Int
--xs = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
xs = Cons 1 (Cons 2 (Cons 3 (Cons 6 Nil)))


ys :: List String
--ys = "alex" `Cons` ("gerdes" `Cons` Nil)
ys = Cons "Alex" (Cons "Gerdes" Nil)


prod :: Num a => List a -> a
prod Nil         = 1
prod (Cons x xs) = x * prod xs

showL :: Show a => List a -> String

showL xs = "[" ++ go xs ++ "]"
    where
        go Nil = ""
        go (Cons x Nil) = show x
        go (Cons x xs) = show x ++ ", " ++ go xs


toList :: [a] -> List a
toList xs = foldr (Cons) Nil xs


data Name = First String | Middle Name String deriving Show


ludvig :: Name
ludvig =  Middle (Middle (First  "Hamster") "Hamster" ) "Ludvig"


data DoubleTree = Leaf String
                | Pair DoubleTree DoubleTree

data TrippleTree = Lov String
                | Cool Int
                | Tripple TrippleTree TrippleTree TrippleTree

{- instance Show TrippleTree where
    show (Lov loveit)                 = loveit
    show (Cool coolint)               = show coolint
    show (Tripple first second third) = "[" ++ show first ++ show second ++ "    " ++ show third
 -}

instance Show TrippleTree where
    show tris = "[" ++ recursivetest tris ++ "]"

recursivetest :: TrippleTree -> String
recursivetest (Lov v) = v
recursivetest (Cool i) = show i
recursivetest (Tripple a b c) = recursivetest a ++ ", " ++ recursivetest b ++ ", " ++ recursivetest c


data Maybe a = Nothing | Just a

testingalittle :: Maybe Int
testingalittle = Just 5


testing :: TrippleTree
testing = Tripple (Tripple (Cool 12) (Lov "Hamster") (Cool 2003)) (Lov "Cool") (Cool 23)




data Car = Model String | Year Car Int deriving Show

test :: Car
test = Year ( Model "Volvo hamster") 2013