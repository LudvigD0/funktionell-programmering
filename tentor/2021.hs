
{- 

thrice :: a -> [a]
thrice x = [x,x,x]

sums :: Num a => [a] -> [a]
sums (x:y:ys) = x : sums (x + y : ys)
sums xs = xs

--map f xs = [ f x | x <- xs ]


map thrice (sums [0..4])
--first evaluate [0..4]
map thrice (sums [0,1,2,3,4])
--def of sums
map thrice (0 : sums (0 + 1 : [2,3,4]))
--calculate 0+1=1 and use def of sums
map thrice (0 : 1 : sums (1 + 2 : [3,4]))
--calculate 1+2=3 and use def of sums
map thrice (0 : 1 : 3 : sums (3 + 3 : [4]))
--calculate 3+3=6 and use def of sums
map thrice (0 : 1 : 3 : 6 : sums (6 + 4 : []))
--calculate 6+4=10 and by def of sums it will return xs, that is [10]
map thrice (0 : 1 : 3 : 6 : [10])
--build list
map thrice (0 : 1 : 3 : [6,10])
--build list
map thrice (0 : 1 : [3,6,10])
--build list
map thrice (0 : [1,3,6,10])
--bulid list
map thrice [0,1,3,6,10]
--use map, by def of map its a list comprehension, thus it will directly apply funciton on all the elements.
[ thrice 0, thrice 1, thrice 3, thrice 6, thrice 10]
--by def of thrice it will create lists with the length of 3 of the same number
[[0,0,0], [1,1,1], [3,3,3], [6,6,6], [10,10,10]]



--evaluates to: 
[[0,0,0], [1,1,1], [3,3,3], [6,6,6], [10,10,10]]




 -}






{- data Root = None | One Double | Two Double Double

roots :: (Double, Double, Double) -> Root

nRoots :: Double -> Double -> Double -> Int
nRoots a b c =  

r
 -}







condMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]

condMap p f xs = [if p x then f x else x | x <- xs]


{-
condMap p f [] = []
condMap p f (x:xs) = (if p x then f x else x) : condMap p f xs

-}
--condMap p f xs = map (\x -> if p x then f x else x) xs

replace :: Char -> Char -> String -> String
replace x y str = condMap (\z -> z == x) (\z -> y) str