

data Color = Red | White | Blue deriving Show

partitionColors :: [Color] -> ([Color], [Color], [Color])
partitionColors xs = helper [] [] [] xs
    


helper red white blue []     = (red, white, blue)
helper red white blue (x:xs) = case x of
            Red   -> helper (x : red) white blue xs
            White -> helper red (x : white) blue xs
            Blue  -> helper red white (x : blue) xs





readPost :: IO (String, String, Int)
readPost = do
    putStrLn "Welcome to FpBook!"

    putStr "What is your name:\n>"
    name <- getLine
    
    putStr "Please enter your message:\n>"
    message <- getLine

    putStr "Who can see the post? (0 = private, 1 = friends, 2 = public)\n>"
    visibility <- getLine
    let visibilityint = read visibility

    if visibilityint < 0 then error "Choose between 0, 1 and 2" 
    else if visibilityint > 2 then error "Choose between 0, 1 and 2"
    else return (name, message, visibilityint)




data Tree = Leaf Int | Node Tree Int Tree deriving (Show, Eq)

t :: Tree
t = Node (Node (Leaf 12) 24 (Leaf 5)) 80 (Leaf 15)

size :: Tree -> Int
size (Leaf x) = 1
size (Node l i r) = size l + 1 + size r





minTree :: Tree -> Int
minTree (Leaf x)     = x
minTree (Node l i r) = min (minTree l) (min (minTree r) i)



toList :: Tree -> [Int]
toList = undefined

prop_size :: Tree -> Bool
prop_size t = size t == length (toList t)

prop_min :: Tree -> Bool
prop_min t = minTree t == minimum (toList t)



data EvenList a = Nil | Add a (EvenList a) a deriving Show

elist :: EvenList Int
elist = Add 3 (Add 2 (Add 1 Nil 10) 20) 30


mapEven :: (a -> b) -> EvenList a -> EvenList b
mapEven f Nil = Nil
mapEven f (Add l x r) = Add (f l) (mapEven f x) (f r)





foldrEven :: (a -> b -> a -> b) -> b -> EvenList a -> b
foldrEven f z Nil = z
foldrEven f z (Add l x r) = f l (foldrEven f z x) r







{- elist :: EvenList Int
elist = Add 3 (Add 2 (Add 1 Nil 10) 20) 30
 -}

type StringF = String -> String

charF :: Char -> StringF
charF c = \s -> c:s

showF :: Show a => a -> StringF
showF x = \s -> show x ++ s

showEvenF :: Show a => EvenList a -> StringF
showEvenF e = charF '[' . go e . charF ']'
    where
        go Nil = id
        go (Add l Nil r) = showF l . charF ',' . showF r
        go (Add l x r)  = showF l . charF ',' . go x . charF ',' . showF r




{- showEvenF :: Show a => EvenList a -> StringF
showEvenF e s = charF '[' (go e (charF ']' s))
    where
        go Nil t = t
        go (Add l Nil r) t = showF l (charF ',' (charF ' ' (showF r t)))
        go (Add l x r) t  = showF l (charF ',' (charF ' ' (go x (charF ',' (charF ' ' (showF r t))))))


 -}