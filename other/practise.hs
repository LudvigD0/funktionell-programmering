

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