{- 

fun []     = 0
fun (x:xs) = if even x then x * fun xs else x + fun xs

-- evaluates to:
-- 42

-- fun [2,21]
-- if even 2 then 2 * fun [21] else 2 + fun [21]
-- if True then 2 * fun [21] else 2 + fun [21]
-- 2 * fun [21]
-- 2 * (if even 21 then 21 * fun [] else 21 + fun [])
-- 2 * (if False then 21 * fun [] else 21 + fun [])
-- 2 * (21 + fun [])
-- 2 * (21 + 0)
-- 2 * 21
-- 42

--b
fun :: [Int] -> Int





 -}


























chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = fst (splitAt n xs) : chop n (snd (splitAt n xs))





 





{- 





--FUPster
data FUPster = FUPster
    {
        players :: [Player]
        , cards :: [Card]
    }

data Card = Card 
    {
        name :: String
        , speciality :: Speciality
        , yearOfBirth :: Int
    }

data Speciality = Programming | LogicAndTypes | Security | FormalMethods

data Player = Player
    {
        name :: String
        , cards :: [Card]
        , fupsters :: Int
    }






 -}












{- readCard :: IO (String, Int, String)
readCard = 
    do
        putStr "Please give the legend's name:\n>"
        name <- getLine
        putStr "Year of birth:\n>"
        yearOfBirth <- getLine

        speciality <- getSpeciality name

        return (name, yearOfBirth, speciality)




getSpeciality :: String -> IO String



 -}







{- 
data List = Empty | Skip List | Cons Int List deriving Show

xs :: List
xs = Cons 1 (Cons 2 (Skip (Cons 4 Empty)))

toList :: List -> [Maybe Int]
toList Empty     = []
toList (Skip xs)   = Nothing : toList xs
toList (Cons i xs) = Just i  : toList xs
  -}
{- 
toList xs = case xs of
    Empty    -> []
    Skip l   -> Nothing : toList l
    Cons i l -> Just i  : toList l -}














prop_chop :: Int -> [Int] -> Bool


prop_chop n xs = let newList = init (chop n xs) in and [length x == n | x <- newList] && length (last newList) <= n
    


{- instance Arbitrary prop_chop where
    arbitrary = prop_chop

 -}












iterateWhile :: (a -> Bool) -> (a -> a) -> a -> a
iterateWhile fb f x = if fb x then iterateWhile fb f (f x) else x







type Test a = String -> a





