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

import           Applicative
import           Base                    hiding ( map )
import           Folds
import           Functor

-- | Takes an unary function and applies it to one element wrapped in a context.
--
-- >>> liftA (+1) (Id 7)
-- Id 8
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA = (<$>)

-- | Takes a binary function and applies it to two elements wrapped in a context.
--
-- >>> liftA2 (+) (Id 7) (Id 8)
-- Id 15
--
-- >>> liftA2 (+) [1, 2, 3] [4, 5]
-- [5,6,6,7,7,8]
--
-- >>> liftA2 (+) (Just 7) (Just 8)
-- Just 15
--
-- >>> liftA2 (+) (Just 7) Nothing
-- Nothing
--
-- >>> liftA2 (+) Nothing (Just 8)
-- Nothing
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Takes a ternary function and applies it to three elements wrapped in a context.
--
-- >>> liftA3 (,,) (Id 7) (Id 8) (Id 9)
-- Id (7,8,9)
--
-- >>> liftA3 (,,) [1,2] [3,4] [5]
-- [(1,3,5),(1,4,5),(2,3,5),(2,4,5)]
--
-- >>> liftA3 (,,) (Just 7) (Just 8) (Just 9)
-- Just (7,8,9)
--
-- >>> liftA3 (,,) (Just 7) Nothing (Just 9)
-- Nothing
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

-- | Turn a list of structured items into a structured list of item
--
-- /Hint/: use 'foldr' and type holes!
--
-- >>> sequence [Id 7, Id 8, Id 9]
-- Id [7,8,9]
--
-- >>> sequence [[1, 2, 3], [1, 2]]
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence [Just 7, Nothing]
-- Nothing
--
-- >>> sequence [Just 7, Just 8]
-- Just [7,8]
sequence :: Applicative f => [f a] -> f [a]
sequence = foldr (liftA2 (:)) (pure [])
-- $setup
-- >>> import Data.Semigroup
-- >>> sieve = (\x -> if even x then Just x else Nothing)
--
-- List monoid under concatenation.
-- >>> :{
-- newtype MList a = MList { elems :: [a] }
-- instance Semigroup (MList a) where
--    (<>) (MList l) (MList m) = MList (l ++ m) -- Concatenation (mappend)
-- instance Monoid (MList a) where
--    mempty = MList []            -- Identity
-- :}

-- | A 'Foldable' is a structure which can be reduced to a single value given a
-- function.
--
-- /Hint/: Use the following /folding/ function.
--
-- > mconcat :: (Monoid m) => [m] -> m
--
-- /Hint/: Use the following "Nil."
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

-- | Given a list with non-monoidal elements, and a function to put them into
-- a monoid, fold the list into the monoid.
--
-- We have to use a "monoid under addition."
--
-- >>> getSum $ foldMap Sum [1..10]
-- 55
--
-- >>> getProduct $ foldMap Product [1..10]
-- 3628800
--
-- MList is also a monoid under concatenation (append).
--
-- >>> elems $ foldMap MList [[1..10], [11..20]]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
instance Foldable [] where
  foldMap :: (Monoid m) => (a -> m) -> [a] -> m
  -- foldMap f = foldr (mappend . f) mempty
  foldMap f l = mconcat (f <$> l)


-- | Traverse a list while producing an effect.
--
-- /Hint/: use the 'sequence' function you implemented in week 8 (see
-- Exercises.hs).
--
-- >>> traverse sieve [2, 4, 6]
-- Just [2,4,6]
--
-- >>> traverse sieve [2, 4, 7]
-- Nothing
instance Traversable [] where
  traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
  traverse f l = sequence $ f <$> l

-- | Write instance for Maybe as a foldable. You can think of it as folding over a list of 0 or 1 elements
--
-- >>> getSum $ foldMap Sum Nothing
-- 0
--
-- >>> getProduct $ foldMap Product (Just 5)
-- 5
--
instance Foldable Maybe where
  foldMap :: (Monoid m) => (a -> m) -> Maybe a -> m
  foldMap f (Just x) = f x
  foldMap _ Nothing  = mempty

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
  traverse _ Nothing  = pure Nothing
  traverse f (Just b) = Just <$> f b

{-
    ******************** OPTIONAL **************************
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
  foldMap _ Nil        = mempty
  foldMap f (Node v l) = mappend (f v) (mconcat (map (foldMap f) l))
  -- foldMap f (Node val l) = (f val) <> foldMap (foldMap f) l


-- | Traverse a 'RoseTree' while producing an effect.
--
-- >>> traverse sieve (Node 4 [Node 6 []])
-- Just (Node 4 [Node 6 []])
--
-- >>> traverse sieve (Node 4 [Node 6 [], Node 7 []])
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
  traverse _ Nil        = pure Nil
  traverse f (Node v l) = Node <$> f v <*> traverse (traverse f) l
  -- traverse f (Node val l) = lift Node (f val) (traverse (traverse f) l)
