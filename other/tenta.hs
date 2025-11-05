import Test.QuickCheck
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = go 1 n xs
    where
        go i n [] = []
        go i n (x:xs) | i == n = go 1 n xs
                      | otherwise  = x : go (i+1) n xs




{- prop_subset :: Int -> [Int] -> Bool
prop_subset n xs | n >= 1    = helper xs (dropEvery n xs)
                 | otherwise = False

helper :: [Int] -> [Int] -> Bool
helper xs []                 = True
helper xs (y:ys) | elem y xs = helper xs ys
                 | otherwise = False
 -}



tmap :: (a -> b) -> (c -> d) -> [(a,c)] -> [(b,d)]
tmap f g []          = []
tmap f g ((x,y):xys) = (f x, g y) : tmap f g xys






data RoseTree a = Node a [RoseTree a] deriving Show

leaf :: a -> RoseTree a
leaf x = Node x []

rt :: RoseTree Int
rt = Node 1 [Node 2 [leaf 4, leaf 5], Node 3 [leaf 6, leaf 7]]

ex = Node 1 [ Node 2 [Node 4 [], Node 5 [] ] , Node 3 [ Node 6 [], Node 7 [] ] ]

flatten :: RoseTree a -> [a]
flatten tree = case tree of
    Node x [] -> [x]
    Node x ys -> [x] ++ helper ys
    
 --flatten y ++ flatten ys
    
helper :: [RoseTree a] -> [a]
helper []     = []
helper (x:xs) = flatten x ++ helper xs
    




{- foldTree :: (a-> [b] -> b) -> RoseTree a -> b
foldTree f tree = foldr f [] (flatten tree) -}