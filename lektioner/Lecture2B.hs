import Prelude hiding (sum, last, reverse, length, Nothing, Maybe, Just)
{-
sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs
-}  


sum [] = 0
sum (x:xs) = x + sum xs


last :: [Int] -> Int
last [x] = x
last (x:xs) = last xs


reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]


length [] = 0
length (_:xs) = 1 + length xs


lenStr :: [String] -> Int
lenStr [] = 0
lenStr (_:xs) = 1 +lenStr xs  












-- define data type

data Maybe a = Just a | Nothing deriving (Show, Eq)


safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y )

maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd (Just x) (Just y) = Just (x + y)
maybeAdd _   _  = Nothing
--



mats = Person "Mats" "Gerdes" 123
lise = Person "Lise " "Gerdes" 213


data Person = Person String String Int
instance Show Person where
    show (Person name lastname soc) = name ++ " " ++ lastname



instance Eq Person where
    Person _ _ s1 == Person _ _ s2 = s1 == s2