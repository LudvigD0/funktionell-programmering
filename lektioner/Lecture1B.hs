-- Return the maximum of two values, now using guards

ma x y | x < y = y
ma x y | x > y = x
ma x y = y

testma x y 
        | x < y     = y
        | x > y     = x
        | otherwise = y



f x | even x = x `div` 2
    | otherwise = 3*x + 1




power n k | k < 0 = error "power: negative exponent"
power n 0 = 1
power n k = n*power n (k-1)


--Computing factorial of a number
-- 5! = 5 * 4 * 3 * 2 * 1 * 1
fac 0 = 1
fac n = n * fac (n-1)