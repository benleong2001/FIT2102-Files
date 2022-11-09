{-# LANGUAGE NoImplicitPrelude #-}

-- Complete the following table when you submit this file:

-- Surname     | Firstname | email | Contribution% | Any issues?
-- =============================================================
-- Person 1... |           |       | 25%           |
-- Person 2... |           |       | 25%           |
-- Person 3... |           |       | 25%           |
-- Person 4... |           |       | 25%           |

-- | The goal of this module is to rewrite "standard" Haskell functions using
-- only /folds/.
module Folds where

import           Base                    hiding ( (++) )

-- | Rewrite 'sum' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> sum [1, 2, 3]
-- 6
--
-- >>> sum [1..10]
-- 55
--
-- prop> \x -> foldl (-) (sum x) x == 0
sum :: Num a => [a] -> a
sum = foldr (+) 0

-- | Rewrite 'product' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> product [1, 2, 3]
-- 6
--
-- >>> product [1..10]
-- 3628800
product :: Num a => [a] -> a
product = foldr (*) 1

-- | Rewrite 'length' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> length [1, 2, 3]
-- 3
--
-- >>> length []
-- 0
--
-- prop> sum (map (const 1) x) == length x
length :: [a] -> Int
length = foldr (const (+ 1)) 0

-- | Rewrite 'map' using 'foldr'.
--
-- >>> map (+ 1) [1, 2, 3]
-- [2,3,4]
--
-- >>> map (* 2) [1, 2, 3]
-- [2,4,6]
--
-- prop> map id x == x
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

-- | Rewrite 'filter' using 'foldr'.
--
-- >>> filter (< 3) [1, 2, 3]
-- [1,2]
--
-- >>> filter even [1, 2, 3, 4, 5]
-- [2,4]
--
-- prop> filter (const True) l == l
--
-- prop> filter (const False) l == []
filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr f' []
 where
  f' x a | f x       = x : a
         | otherwise = a

-- | Rewrite /append/ '(++)' using 'foldr'.
--
-- | Must write this in point-free notation and without lambda functions
--
-- >>> [1] ++ [2] ++ [3]
-- [1,2,3]
--
-- >>> "abc" ++ "d"
-- "abcd"
--
-- prop> (x ++ []) == x
--
-- Associativity of append.
-- prop> (x ++ y) ++ z == x ++ (y ++ z)
(++) :: [a] -> [a] -> [a]
(++) = flip $ foldr (:)

-- | Rewrite 'all' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> all [True, True, True]
-- True
--
-- >>> all [False, True, True]
-- False
all :: [Bool] -> Bool
all = foldr (&&) True

-- | Rewrite 'any' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> any [False, False, False]
-- False
--
-- >>> any [False, True, False]
-- True
any :: [Bool] -> Bool
any = foldr (||) False

-- | Flatten a (once) nested list.
-- | Must write point-free and without lambda functions.
-- >>> flatten [[1], [2], [3]]
-- [1,2,3]
--
-- >>> flatten [[1, 2], [3], []]
-- [1,2,3]
--
-- prop> sum (map length x) == length (flatten x)
flatten :: [[a]] -> [a]
flatten = foldr (++) []
