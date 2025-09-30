

{- 

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

(&&) :: Eq a => a -> a -> a
b && c | b == c = b

test' :: [Char] -> Bool
test' ['a',_,_] = True
test' _         = False

test'' :: [Char] -> Bool
test'' ('a':_) = True
test'' _       = False

testing' = \x -> x + x

add'''' :: Int -> Int -> Int
add'''' x y = x + y

add''''' :: Int -> (Int -> Int)
add''''' = \x -> (\y -> x + y)

odds :: Int -> [Int]
odds n = map f [0..n-1]
    where f x = x*2 + 1

odds' :: Int -> [Int]
odds' n = map (\x -> x*2 + 1) [0..n-1]

(#) = \x -> (\y -> x # y)



halve :: [a] -> ([a], [a])
halve xs | even (length xs) = splitAt (length xs `div` 2) xs 
         | otherwise = error "List is not even"

third :: [a] -> a
third xs = head (tail (tail xs))

thirdl :: [a] -> a
thirdl xs = xs !! 2

thirdr :: [a] -> a
thirdr (_:_:x:_) = x




safetailc :: [a] -> [a]
safetailc xs = if null xs then [] else tail xs

safetailg :: [a] -> [a]
safetailg xs | null xs = []
                 | otherwise = tail xs

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

(||) :: Bool -> Bool -> Bool
--True || True = True
--True || False = True
--False || True = True 
--False || False = False

True || b = True
False || _ = False

(<--->) :: Bool -> Bool -> Bool
a <---> b = if a then
     if b then True else False
      else False 

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))



luhnDouble :: Int -> Int
luhnDouble x
  | calc > 9  = calc - 9
  | otherwise = calc
  where calc = x * 2


luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z e = mod (sum [luhnDouble x,y,luhnDouble z,e]) 10 == 0

--list comprehension
--1.

sum'''' = sum [x^2 | x <- [1..100]]

--2.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n], 0 <= x, x <= m, 0 <= y, y <= n]

--3.
square :: Int -> [(Int, Int)]
square z = [(x,y) | (x,y) <- grid z z, x /= y]


--4.
-- replicate 3 True

replicatefunction x y = [y | x <- [0..x-1]]

--5.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]
 -}
--6.
perfects :: Int -> [Int]
perfects n = [x | x <- [0..n], x > 0, sum (factors x) == x]

factors n = [x | x <- [1..n-1], n `mod` x == 0]

--7.
-- [(x,y) | x <- [1,2], y <- [3,4]]

-- []

zipit = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]


find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

findit n ms = find n (zip ms [1..]) 

scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]


--recursive functions

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)


insert :: Ord a => a -> [a] -> [a]
insert x [] = []
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys


zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


--1.
fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac (n-1)



sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)


x ^^^ 0 = 1
x ^^^ y = x * (x ^^^ (y-1))

euclid :: Int -> Int -> Int

euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | y < x = euclid (x-y) x




--init 
-- init [2,3,4]
-- 2 : init [3,4]
-- 2 : 3 : init [4]
-- 2 : 3 : []
-- 2 : [3]
-- [2, 3]



and' :: [Bool] -> Bool
and' [x]     = x
and' (x:xs) = x && and' xs


concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n m = m : replicate' (n-1) m

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)


elem' :: Eq a => a -> [a] -> Bool
elem' n (x:xs) | n == x = True
               | otherwise = elem' n xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys) | x <= y    = x:merge' xs (y:ys)
                     | otherwise = y:merge' (x:xs) ys


twice :: (a -> a) -> a -> a
twice f x = f (f x)



sumt :: Num a => [a] -> a
sumt = sum' 0
    where
        sum' v [] = v
        sum' v (x:xs) = sum' (x+v) xs



f `o` g = \x -> f (g x)

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
            where weights = iterate (*2) 1


--exercises
--[f x | x <- xs, p x]
--map f (filter p xs)

--2.
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x: takeWhile' p xs
                    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

--3.
-- map f
mapmo f 