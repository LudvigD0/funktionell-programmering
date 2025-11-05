import Test.QuickCheck
import Data.List
f x y = if x > y then x + y else y











{- helper :: IO String
helper =
    do
        putStr "What is the player's name?:\n>"
        name <- getLine
        return name


teamSelection :: IO [String]
teamSelection =
    do
        putStr "Welcome coach! How many players do you want to select?\n>"
        n <- readLn
        listOfNames <- replicateM n helper

        return listOfNames
 -}



data Tree = Leaf Int | Node Tree Int Tree deriving (Eq, Show)


t :: Tree
t = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)



root :: Tree -> Int
root (Node l x r) = x
root (Leaf x)     = x




mirror :: Tree -> Tree
mirror (Leaf x)     = Leaf x
mirror (Node l x r) = Node (mirror r) x (mirror l)













prop_mirror :: Tree -> Bool
prop_mirror t = mirror (mirror t) == t


prop_root :: Tree -> Bool
prop_root t = (root t) == root (mirror t)       




zipMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> [c]
zipMaybe f (x:xs) (y:ys) = case (f x y) of
        Just x    -> x : zipMaybe f xs ys
        Nothing -> zipMaybe f xs ys
zipMaybe _ _ _ = []
    





        








data Board a = Board { size :: Int, rows :: [[a]] }

data Mark = X | O | B deriving Show

example :: Board Mark
example = Board 3 [ [ O, X, B ]
                  , [ X, O, B ]
                  , [ X, B, O ] ]






instance Show a => Show (Board a) where
    show :: Board a -> String                                   
    show (Board size (x:xs)) = unlines (intersperse (replicate (size+2) '-') (go size (x:xs)))
        where
            
            --go :: [[Mark]] -> [String]
            go n [] = []
            go n (y:ys) = (intersperse '|' (row y)) : go n ys
                where
                    --row :: [Mark] -> String 
                    row [] = ""
                    row (z:zs) = show z ++ row zs




example = Board 3 [ [ O, X, B ]
                  , [ X, O, B ]
                  , [ X, B, O ] ]

genBoard :: Int -> Gen (Board Mark)
genBoard n = do
    rows <- vectorOf n (vectorOf n (elements [X, O, B]))
    return (Board n rows)








