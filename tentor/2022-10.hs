f :: [a] -> [[a]] -> [[a]]
f xs (ys:zs:zss) = ys : xs : f xs (zs:zss)
f _  xss         = xss

g :: [a] -> [[a]] -> [a]
g xs xss = concat (f xs xss)








gcd' :: Int -> Int -> Int
gcd' d 0 = d
gcd' x y = gcd y (x `mod` y)




howMuch = Question "Is the inflation above 2 percent?" (Question "Do you have a mortgage?" (Decision 1000) (Decision 400)) (Decision 100)



data DTree a = Decision a | Question String (DTree a) (DTree a) deriving (Eq)


mapDecision :: (a -> b) -> DTree a -> DTree b
mapDecision f (Decision a) = (Decision (f a))
mapDecision f (Question str l r) = (Question str (mapDecision f l) (mapDecision f r))






takeDecision :: DTree a -> IO a

        
takeDecision t = case t of
    (Decision a)          -> return a
    (Question str yes no) -> do
        putStr (str ++ " yes / no\n>")
        answer <- getLine
        if answer == "yes" then takeDecision yes else takeDecision no






lambda :: BillBoard
lambda = BB (4, 10) [(0,2),(1,3),(2,2),(2,4),(3,1),(3,5)]

type Pixel = (Int, Int)

data BillBoard = BB { size :: (Int, Int), actives :: [Pixel] }

instance Show BillBoard where
    show (BB (x,y) ac) = go 0 [(x,y) | x <- [0..x], y <- [0..y]]
        where
            go _ []      = ""
            go n (f:fs) | n == (y+1)    = "\n" ++ if elem f ac then '#' : go  1 fs else '.' : go  1 fs
                           | otherwise = if elem f ac then '#' : go  (n+1) fs else '.' : go (n+1) fs

inactives :: BillBoard -> Int
inactives (BB (x,y) ac) = x * y - (length ac)


invert :: BillBoard -> BillBoard
invert (BB (x,y) ac) = BB (x,y) [(x,y) | x <- [0..x], y <- [0..y], not (elem (x,y) ac)] 







prop_odd :: (Int, Int) -> Bool
prop_odd (x,y) = odd x && odd y || even x && odd y || odd x && even y


prop_lcm :: (Int, Int) -> Bool
prop_lcm (x,y) = lcm x y == x * y






data Customer = Customer
    {
        name :: String
        , phone :: Int
        , plan :: Plan
    }

data PhoneCompany = PhoneCompany
    {
        customers :: [Customer]
        , vat :: Int
    }    


data Plan = PrePaid Int | Subscription Int Int




annasSida :: HTML
annasSida =
    [ Text "Welcome to my website!"
    , Open "P"
        , Open "B"
            , Text "My hobbies are ", Open "EM", Text "Haskell", Close "EM"
            , Text " programming and playing ", Open "EM", Text "Myst", Close "EM"
            , Text "."
        , Close "B"
    , Close "P"
    , Open "P"
        , Text "Thanks for visiting! ", Open "EM", Text "anna@gmail.com", Close "EM"
        , Open "P", Text ". Bye, bye!", Close "P"
    , Close "P"
    ]


type HTML = [Tag]


data Tag
    = Text String
    | Open String
    | Close String
    deriving (Eq, Show)


render :: HTML -> String
render html = go html
    where
        go [] = ""
        go (x:xs) = case x of
            (Text a)  -> a ++ go xs
            (Open a)  -> '<' : a ++ ">" ++ go xs
            (Close a) -> '<' : '/' : a ++ ">" ++ go xs





checkHtml :: HTML -> Bool
checkHtml html = go [] html
    where
        go xs []     = if null xs then True else False
        go xs (y:ys) = case y of 
            (Text a)  -> go xs ys
            (Open a)  -> go (a:xs) ys
            (Close a) -> if (head xs) == a then go (tail xs) ys else False



