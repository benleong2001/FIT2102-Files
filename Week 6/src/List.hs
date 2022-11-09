-- | Custom list implementation.
module List where

import Prelude
import Data.List(sort)

-- | A 'List' contains extra information about its size, and minimum and maximum
-- elements.
data List = List {size :: Int, elems :: [Int], low :: Int, high :: Int}
  deriving(Show)

-- $setup
-- >>> import Data.List(sort)
-- >>> list = (List 7 [1, 7, 9, 2, 6, 11, 3] 1 11)

-- | Create a `List` instance from a list of elements.
--
-- >>> fromList [1, 7, 9, 2, 6, 11, 3]
-- List {size = 7, elems = [1,7,9,2,6,11,3], low = 1, high = 11}

-- Converting a Haskell list into a List
-- Find size with length, low with minimum, high with maximum
fromList :: [Int] -> List
fromList l = List {size = length l, elems = l, low = minimum l, high = maximum l}

-- | Sort the list of elements in a list
--
-- >>> sortList list
-- List {size = 7, elems = [1,2,3,6,7,9,11], low = 1, high = 11}
--
-- prop> elems (sortList (List a l b c)) == sort l

-- Sort a List using sort on elems
sortList :: List -> List
sortList (List {size=s, elems=e, low=l, high=h}) = List {size = s, elems = sort e, low = l, high = h}

-- | Add an element to a list.
--
-- >>> sortList $ addElem 4 list
-- List {size = 8, elems = [1,2,3,4,6,7,9,11], low = 1, high = 11}
--
-- >>> sortList $ addElem 13 list
-- List {size = 8, elems = [1,2,3,6,7,9,11,13], low = 1, high = 13}
--
-- >>> sortList $ addElem 0 list
-- List {size = 8, elems = [0,1,2,3,6,7,9,11], low = 0, high = 11}

-- Appending a new element to elems using ++
addElem :: Int -> List -> List
addElem elem (List {size=s, elems=e, low=l, high=h}) = List {size = s + 1, elems = e ++ [elem], low = if l < elem then l else elem, high = if h > elem then h else elem}

-- | Returns the longest of two lists.
--
-- >>> longest list (fromList [1, 2, 3])
-- List {size = 7, elems = [1,7,9,2,6,11,3], low = 1, high = 11}
--
-- >>> longest list (fromList [1..10])
-- List {size = 10, elems = [1,2,3,4,5,6,7,8,9,10], low = 1, high = 10}

-- Comparing the sizes of the Lists to return the longest of the 2 Lists
longest :: List -> List -> List
longest (List {size=s1, elems=e1, low=l1, high=h1}) (List {size=s2, elems=e2, low=l2, high=h2}) = 
  if s1 > s2 
    then (List {size=s1, elems=e1, low=l1, high=h1}) 
    else (List {size=s2, elems=e2, low=l2, high=h2})
