{- |
Module      : Lab1
Description : Skeleton for lab 1: Power to the People
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <Ludvig Dahlgren, Elliot Frost, Gabriel Hassan>
Lab group   : <51>
-}

module Lab1 where

-- The power function uses explicit recursion to calculate n^k. We developed
-- this function during a lecture.
power :: Int -> Int -> Int
power n k
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part A ----------------------------------------------------------------------

-- Evaluate the expression `power 3 2` by hand. Write the evaluation steps in
-- the comment below by replacing the dots. We have already done the first step
-- for you.

-- power 3 2
-- => { definition of power, third case }
-- 3 * power 3 (2 - 1)
-- => { apply subtraction }
-- 3 * power 3 1
-- => { third case }
-- 3 * 3 * power 3 (1 - 1)
-- => { apply subtraction }
-- 3 * (3 * power 3 0)
-- => { second case }
-- 3 * (3 * 1)
-- => { apply multiplication }
-- 3 * 3
-- => { apply multiplication }
-- 9

-- Part B ----------------------------------------------------------------------

power1 :: Int -> Int -> Int
power1 n k
  | k < 0 = error "power: negative argument"
  | otherwise = product( replicate k n )


-- Part C ----------------------------------------------------------------------



power2 :: Int -> Int -> Int
power2 n 0 = 1
power2 n k
 | k < 0 = error "power: negative argument"
 | even k = power2 (n*n) (k `div` 2)
 | odd k = n * power2 n (k-1)




-- Part D ----------------------------------------------------------------------

-- Part D.1 - describe your test cases here

test1 = power1 2 3 == power 2 3 
test2 = power2 2 3 == power 2 3 --compare power2 with power to ensure the result is the same
test3 = power2 10 10 == power 10 10 --Test that bigger values work
test4 = power2 (-1) 2 == power 1 2  -- testing negative value with positive
test5 = power1 5 0 == power2 8 0 --The zero makes both of the functions return 1 and 1 == 1 is ofcourse true

-- Part D.2
comparePower1 :: Int -> Int -> Bool
comparePower1 n k = power n k == power1 n k

comparePower2 :: Int -> Int -> Bool 
comparePower2 n k = power n k == power2 n k


-- Part D.3

--
nlist = [100,99..1]
klist = [1..100] 
-- Writing this outside for cleaner code and if we wanna reuse it somewhere else in the future

testAll :: Bool
testAll = and (
        [test1, test2, test3, test4, test5] ++ 
        [comparePower1 n k && comparePower2 n k | n <- nlist, k <- klist])

