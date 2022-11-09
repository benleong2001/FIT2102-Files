-- | Implementation of integer valued binary trees.
module BinTree where


-- | A 'BinTree' is a type of tree which is either a 'Leaf' with no values, or
-- a 'Label' with a value and two sub-trees.
data BinTree = Leaf | Label Int BinTree BinTree
  deriving (Show)

-- $setup
-- >>> tree = Label 16 (Label 23 Leaf (Label 73 Leaf Leaf)) (Label 42 Leaf Leaf)
-- >>> one = Label 1 Leaf Leaf

-- | Find the depth of a tree (number of levels)
--
-- >>> depth Leaf
-- 0
--
-- >>> depth (Label 1 Leaf Leaf)
-- 1
--
-- >>> depth tree
-- 3
depth :: BinTree -> Int
-- Base case, when the input is just a Leaf
depth Leaf = 0
-- Only consider the depth of the deepest child
depth (Label root left right) = if depth left > depth right then 1 + depth left else 1 + depth right
dwpth (Label root left right )| depth left> depth right = 1 + depth left
                              | otherwise = 1 + depth right

-- | Find the number of nodes in a tree.
--
-- >>> size Leaf
-- 0
--
-- >>> size one
-- 1
--
-- >>> size tree
-- 4
size :: BinTree -> Int
-- Base case, when the input is just a Leaf
size Leaf = 0
-- Count the number of nodes on each child
size (Label root left right) = 1 + size left + size right

-- | Sum the elements of a numeric tree.
--
-- >>> sumTree Leaf
-- 0
--
-- >>> sumTree one
-- 1
--
-- >>> sumTree tree
-- 154
--
-- prop> sumTree (Label v Leaf Leaf) == v
sumTree :: BinTree -> Int
-- Base case, when the input is just a Leaf
sumTree Leaf = 0
-- Sum the root and the sum of each child
sumTree (Label root left right) = root + sumTree left + sumTree right

-- | Find the minimum element in a tree.
--
-- E.g., minTree @(your pattern here)@ = error "Tree is empty"
--
-- >>> minTree one
-- 1
--
-- >>> minTree tree
-- 16
--
minTree :: BinTree -> Int
-- Base case, when the input is just a Leaf
-- Since we are finding the min, the base value must be very high
minTree Leaf = 10000000000
-- We only consider the smaller of the smallest node for each child
minTree (Label root left right) = let
  -- Find the smaller of smallest here
  smallest = if minLeft < minRight then minLeft else minRight

  in
    if root < smallest then root else smallest

  where
    -- Find the smallest node for each child here
    minLeft = minTree left
    minRight = minTree right

-- | Map a function over a tree.
--
-- >>> mapTree (+1) Leaf
-- Leaf
--
-- >>> mapTree (*1) one
-- Label 1 Leaf Leaf
--
-- >>> mapTree ((flip mod) 2) tree
-- Label 0 (Label 1 Leaf (Label 1 Leaf Leaf)) (Label 0 Leaf Leaf)
mapTree :: (Int -> Int) -> BinTree -> BinTree
-- Base case, when the input is just a Leaf
mapTree f Leaf = Leaf
-- Traverse the entire tree and map each "root" value
mapTree (f) (Label root left right) = Label (f root) (mapTree f left) (mapTree f right)