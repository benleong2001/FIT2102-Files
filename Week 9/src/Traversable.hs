{-# LANGUAGE NoImplicitPrelude, InstanceSigs #-}
-- | This module implements the 'Foldable' and 'Traversable' instances for some
-- recursive types.
--
--  * A 'Foldable' is a structure that can be reduced to a single element by a
--    function.
--
--  * A 'Traversable' is a structure that can be traversed using a function
--    which produces an effect.
module Traversable where

import           Base
import           Functor
import           Applicative
import           Folds
import           ExercisesW8

-- $setup
-- >>> import Data.Semigroup
-- >>> justEven x = if even x then Just x else Nothing

-- | A 'Foldable' is a structure which can be reduced to a single value given a
-- function.
--
-- /Hint/: These are some useful functions given to us by the Foldable typeclass!
--
-- > mconcat :: (Monoid m) => [m] -> m
--
-- > mempty :: Monoid a => a
class Foldable f where
  foldMap :: (Monoid m) => (a -> m) -> f a -> m

-- | A 'Traversable' is a structure which can be /traversed/ while applying an
-- effect. Basically, it is a 'Foldable' with a 'Functor' instance.
--
-- /Hint/: You have to traverse __and__ apply an effect.
class (Functor t, Foldable t) => Traversable t where
  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)

{- ================================================================================= -}
{- ================================================================================= -}

-- | Given a list with non-monoidal elements, and a function to put them into
-- a monoid, fold the list into the monoid.
--
-- /Hint/: What function is available for Foldable that lets us combine a list of Monoids?
--
-- We have to use a "monoid under addition."
--
-- >>> getSum $ foldMap Sum [1..10]
-- 55
--
-- >>> getProduct $ foldMap Product [1..10]
-- 3628800
instance Foldable [] where
    foldMap :: (Monoid m) => (a -> m) -> [a] -> m
    foldMap f = mconcat . (map) f
    {- Map a function to convert the items to Monoids.
        Then combine everything into a single Monoid with mconcat. -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Traverse a list while producing an effect.
--
-- /Hint/: use the 'sequence' function you implemented in week 8 (see
-- Exercises.hs).
--
-- Contains all evens
-- >>> traverse justEven [2, 4, 6]
-- Just [2,4,6]
--
-- Doesn't contain all evens
-- >>> traverse justEven [2, 4, 7]
-- Nothing
instance Traversable [] where
    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse f = sequence . map f
    {- Map a function to convert items to values wrapped by Applicative type f.
        Then use sequence to remove wrapping of items, and only wrap the outer most layer.
        e.g., [(f v1), (f v2), (f, v3), ...] --> f [v1, v2, v3, ...] -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Write instance for Maybe as a foldable.
--
-- /Hint/: What is mempty?
--
-- >>> getSum $ foldMap Sum Nothing
-- 0
--
-- >>> getProduct $ foldMap Product (Just 5)
-- 5
instance Foldable Maybe where
    foldMap :: (Monoid m) => (a -> m) -> Maybe a -> m
    foldMap f (Just a) = f a
    foldMap _ _ = mempty -- mempty = the Monoid version of Nothing

{- ================================================================================= -}
{- ================================================================================= -}

-- | Traverse a Maybe
--
-- >>> traverse (\x -> [x, x+1]) (Just 5)
-- [Just 5,Just 6]
--
-- >>> traverse (\x -> [x, x+1]) Nothing
-- [Nothing]
--
instance Traversable Maybe where
    traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse fun (Just a) = pure pure <*> fun a
    traverse _ Nothing = pure Nothing
    {- EXPLANATION: 
        fun a returns the value b wrapped by f: fun a = f b
        We want to return f (Maybe b). This can be done by accesing the b within f b and apply the pure function onto it.
        *Recall: pure b = Just b, for Maybe Applicative

        To access b within f b, we must somehow get into the f wrapping. This is done using the apply function, <*>.
        *Recall: (f func) <*> (f value) = f (func value)

        For this to work, the input function must also be wrapped by f. 
            Hence, we make use of pure once again to wrap the other pure function with f
            pure pure --> wrap 'pure' (the one to convert b to Just b) with Applicative f
            (Equivalently, we could use pure Just, but pure pure is funnier)
    -}

{-
    ******************** Supplementary **************************
-}

-- Now unto rose trees.

-- | Fold a RoseTree into a value.
--
-- >>> getSum $ foldMap Sum (Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]])
-- 17
--
-- >>> getProduct $ foldMap Product (Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]])
-- 168
--
-- /Hint/: use the Monoid's 'mempty', 'mappend' and 'mconcat'.
instance Foldable RoseTree where
    foldMap :: (Monoid m) => (a -> m) -> RoseTree a -> m
    foldMap f (Node v vs) = f v <> mconcat (map (foldMap f) vs)
    {- RoseTrees have 2 sections, Node value and children
        For Node value:
            Just apply the function to value.
            
        For children:
            We need to map the function all child Trees 
            Since the child are RoseTrees and not Nodes, we map the foldMap function onto them.
            Once the function is applied, we combine their values with mconcat.

        Lastly, we combine node and children values with mappend, <> -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Traverse a 'RoseTree' while producing an effect.
--
-- >>> traverse justEven (Node 4 [Node 6 []])
-- Just (Node 4 [Node 6 []])
--
-- >>> traverse justEven (Node 4 [Node 6 [], Node 7 []])
-- Nothing
--
-- /Hint/: follow the types, use type holes to try to figure out what goes where.
--
-- /Hint (spoiler!)/: pattern match on Node to pull out the value and the list
-- of child rosetrees then, you need to lift (as discussed in week 8) the
-- 'Node' constructor over (the traversing function applied to the value) and
-- (the result of traversing a function over the list of child rosetrees).
--
-- /Hint (more spoiler!)/: the function you traverse over the child rosetrees,
-- will itself be traversing a function over a rosetree.
--
-- Note: if even after reading all the hints and spoilers you are still
-- completely mystified then write down questions for your tutor and your best
-- approximation in English of what you think needs to happen in English.
instance Traversable RoseTree where
    traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
    traverse f (Node v vs) = liftA2 (Node) (f v) (traverse (traverse f) vs)

{- EXPLANATION:
    The Node constructor takes in 2 inputs:
        - A value
        - A list of RoseTrees
    
    When applying the function input to the RoseTree values, 
        we end up with the results wrapped in the Applicative type f.
        However, we only want f to be on the very outside of the RoseTree result.
        Hence, we use liftA2.

    liftA2 uses the Node constructor as its function, where the 2 inputs are:
        - The resulting Node value (wrapped by f)
            - Just apply the function to the root value.
        - The resulting child list (wrapped by f)
    
    HOW TO GET NEW CHILD LIST?
    Original Child List: [RoseTree t1, RoseTree t2, ...]                    t = Tree
                            = [(Node v1 [...]), (Node v2 [...]), ...]       v = Node value
    We can map the list with our function... 
        but that wouldn't work since each item is a RoseTree, not a Node value.
        Plus, we want the Trees to be wrapped by f. So, traverse is used instead.
        We need to map a traverse f to all RoseTree children. This will results in:
            [f (Node r1 [...]), f (Node r2 [...]), ...]                     
                r = resulting Node value, and each Tree is wrapped by f
        Now, we need to "lift" the f from every child so that it is only wrapping the list (of children).
        Same logic as wrapping the Trees by f, we use traverse instead of map.
        In the end, we get: traverse(traverse(f))(children) --> f [(Node r1 [..]), (Node r2 [..]), ..]

    Finally, putting everything together, we will form the final Tree 
        using the Node constructor and the liftA2 function.
        The purpose of the liftA2 function is to 'lift' the 
            f wrapping from the parameters and apply it to the returning Tree only
            - liftA2 (func) (f param1) (f param2) --> f result
    -}
