module Examples where

import           Prelude
import           Data.List
import           Data.Ord
import           Data.Function

-- $setup

-- | Sorting a list by length.
--
-- >>> sortList [[1,2,3], [1,2]]
-- [[1,2],[1,2,3]]
sortList :: [[a]] -> [[a]]
sortList = sortOn (length)

-- | Finding the largest list.
--
-- >>> longestList [[1,2,3], [1,2]]
-- [1,2,3]
longestList :: [[a]] -> [a]
longestList = head . reverse . sortList

-- | Finding the sumProduct of two lists.
--
-- >>> sumProduct [1,2,3] [1,2,4]
-- 17
sumProduct :: [Int] -> [Int] -> Int
sumProduct l1 = sum . zipWith (*) l1

-- | Group consecutive elements which are equal after applying function
--
-- >>> groupEqual id [1,1,1,1,2,2,2,2,2,3,4]
-- [[1,1,1,1],[2,2,2,2,2],[3],[4]]
-- >>> groupEqual (`mod` 5) [5,5,5,5,10,10,10,15,15,15]
-- [[5,5,5,5,10,10,10,15,15,15]]
groupEqual :: Eq b => (a -> b) -> [a] -> [[a]]
groupEqual = groupBy . on (==)

-- | Apply function to every element in a nested list
--
-- >>> nestedMap (+1) [[1,2,3], [4,5,6], [7,8,9]]
-- [[2,3,4],[5,6,7],[8,9,10]]
nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap = map . map 
