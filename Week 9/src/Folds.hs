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

import           Base hiding ((++))

-- | Rewrite 'all' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> all [True, True, True]
-- True
--
-- >>> all [False, True, True]
-- False
all :: [Bool] -> Bool
all = foldr (&&) True -- Equivalent to reduce(func=AND)(init=True). This takes in a list as input.

-- | Rewrite 'any' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> any [False, False, False]
-- False
--
-- >>> any [False, True, False]
-- True
any :: [Bool] -> Bool
any = foldr (||) False -- Equivalent to reduce(func=OR)(init=False). This takes in a list as input.

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
sum = foldr (+) 0 -- Equivalent to reduceRight(func=\x y->x+y)(init=0). This takes in a list as input.

-- | Rewrite 'product' using 'foldr'.
-- | Must write point-free and without lambda functions.
-- >>> product [1, 2, 3]
-- 6
--
-- >>> product [1..10]
-- 3628800
product :: Num a => [a] -> a
product = foldr (*) 1 -- Equivalent to reduceRight(func=\x y->x*y)(init=1). This takes in a list as input.

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
length = foldr ((+) . (const 1)) 0 -- Equivalent to reduceRight(func=\x _->x+1)(init=0). This takes in a list input.
                                    -- (const a b) returns a for all inputs b

-- | Rewrite /append/ '(++)' using 'foldr'.
-- | Must write this in point-free notation and without lambda functions
--
-- /Hint/: This is the same as cons-ing the elements of the first list to the second
-- /Hint/: The 'flip' function might come in handy
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
(++) = flip $ foldr (:) -- Equivalent to reduceRight(func=cons)(init=list2)(list=list1)
                        -- Adds elements of list 1 (traverse right to left) into the front of list 2
                        {- [1, 2, 3] ++ [4, 5, 6]
                            --> [1, 2] ++ [3, 4, 5, 6]
                            --> [1] ++ [2, 3, 4, 5, 6]
                            --> [1, 2, 3, 4, 5, 6]
                        -}

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
flatten = foldr (++) [] -- Equivalent to reduceRight(func=concat)(init=[]). This function accepts an input list.
                        -- Essentially, the function combines all inner lists using (++) to concat them
