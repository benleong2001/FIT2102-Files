-- | Example questions using hoogle


module Examples where

import Prelude

-- $setup

-- | Calculate eulerProblem1
--
-- >>> eulerProblem1 1000
-- 233168

-- From a range of numbers from 1 to 999,
-- filter them based on their divisibility
-- taking only multiples of 3 or 5
-- then reducing them using fold and adding them up
eulerProblem1 :: Int -> Int
eulerProblem1 n = foldl (+) 0 $ filter (\x -> mod x 3 == 0 || mod x 5 == 0) [1..n-1]

-- | Function to check if every element in a list is even.
-- Avoid hard coding recursion in these functions, if you do, you will lose marks!
--
-- >>> allEvens [1,2,3,4,5]
-- False
-- >>> allEvens [2,4]
-- True

-- Using in-built all function to check if all the elements are even
allEvens :: [Int] -> Bool
allEvens = all even

-- | Function to check if any element is odd
-- Avoid hard coding recursion in these functions, if you do, you will lose marks!
--
-- >>> anyOdd [1,2,3,4,5]
-- True
-- >>> anyOdd [0,0,0,4]
-- False

-- Using in-built any function to check if any of the elements are odd
anyOdd :: [Int] -> Bool
anyOdd l = any odd l

-- | Function to sum every element in two lists
-- Avoid hard coding recursion in these functions, if you do, you will lose marks!
--
-- >>> sumTwoLists [1,2,3,4,5] [1,2,3,4,5]
-- [2,4,6,8,10]

-- Adding each element of one list to their corresponding element in another list
-- This is done recursively
sumTwoLists :: [Int] -> [Int] -> [Int]
sumTwoLists l1 l2 = zipWith (+) l1 l2 

-- | Function to make a list of the first item of each pair in a list of pairs
-- Avoid hard coding recursion in these functions, if you do, you will lose marks!
--
-- >>> firstItem [(2,1), (4,3), (6,5)]
-- [2,4,6]

-- Recursively take the first item only and connect it to the next call
-- using ++ and recursion
firstItem :: [(a,b)] -> [a]
firstItem l1 = map fst l1
