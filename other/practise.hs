

qsort [] = []

qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]


--give another possible calculation for the result of double (double 2)
double x = x + x

double' k n = k * double (n-1)

--wrong answer above, I was just supposed to write down the steps the function takes when it doubles the things.

--show that sum [x] = x

-- sum [x]
-- x + sum []
-- x + 0


--define a function product that produces the product of a list of numbers,
-- and show using you definition that product [2,3,4] = 24.Applicative
    


product' [] = 1
product' (x:xs) = x * product' xs



--4. How should the defintion of the function qsort be
-- modified so that it produces a reverse sorted version of a list?

qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]


--what would be the effect of replacing <= by < in the original defintion of qsort?
-- Hint: consider the example qsort [2,2,3,1,1].

qsort'' [] = []
qsort'' (x:xs) = qsort'' larger ++ [x] ++ qsort'' smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

--  2-3+4
--  (2-3)+4

--  2^3^4
--  2^(3^4)


--head [2,3,54]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' xs = go xs []
    where 
        go [] acc = acc
        go (y:ys) acc = go ys (y:acc)




a = b + c
    where 
        {b = 1;
        c = 2};
d = a * 2    


average ns = div (sum ns) (length ns)

average' ns = sum ns `div` length ns

--(2^3)*4
--(2*3)+(4*5)
--2+(3*(4^5))


n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

lastnumber :: [Int] -> Int
lastnumber [] = 0
lastnumber xs = xs !! max 0 (length xs - 1)

lastnumber' xs = head(drop (length xs - 1) xs)


removelast xs = take (length xs - 1) xs

removelast' [] = []
removelast' [_] = []
removelast' (x:xs) = x : removelast' xs



--types and classes



test = ([2,3], ["test","test"])

add' :: Int -> (Int -> Int)
add' x y = x + y



--['a', 'b', 'c'] ::[Char]
--('a', 'b', 'c') :: (Char,Char,Char)
--[(Bool, Char)]
--([Bool], [Char])
--[([a] -> [a])]

bools :: [Bool]
bools = [True, False, False]

nums :: [[Int]]
nums = [[2,3,4],[2,3]]

add :: Int -> Int -> Int -> Int
add x y z = x*y*z

copy :: a -> (a,a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x =  f x

id' :: Num a => a -> a
id' x = x + 1

id'' :: a -> a
id'' x = x

const' :: a -> b -> a
const' x  _ = x

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

snd' :: (a,b) -> b
snd' (a,b) = b

fst' :: (a,b) -> a
fst' (a,b) = a

singleton :: a -> [a]
singleton a = [a]

doubleApply :: (a -> a) -> a -> a
doubleApply f x = f (f x)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

applyBoth :: (a -> b) -> (a, a) -> (b, b)
applyBoth f (x, y) = (f x, f y)

map' :: (a -> b) -> [a] -> [b]
--map' f xs = [f t | t <- xs]

map' _ [] = []
map' f (x:xs) = f x : map' f xs

maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply _ Nothing = Nothing
maybeApply f (Just a) = Just (f a)

eitherLength :: Either [a] [b] -> Int
eitherLength (Left xs) = length xs
eitherLength (Right ys) = length ys

listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' (x:xs) = Just x 

headOr :: a -> [a] -> a
headOr x ys = x

choose :: (a -> Bool) -> a -> a -> a
choose f x y = if f x then x else y

applyList :: [a -> b] -> a -> [b]
applyList fs x = [f x | f <- fs]

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

join :: (a -> a -> a) -> [a] -> a
join f (x:xs) = f x (join f xs)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

second :: [a] -> a
second xs = head (tail xs)

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)

pair' :: a -> b -> (a, b)
pair' x y = (x, y)

double''' :: Num x => x -> x
double''' x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice'' :: (a -> a) -> a -> a
twice'' f x = f (f x)





