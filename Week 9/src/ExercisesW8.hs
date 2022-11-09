{-# LANGUAGE NoImplicitPrelude #-}
-- | Convenience functions for applicative functors.
module ExercisesW8 where

import           Base
import           Functor
import           Applicative

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

-- | Replicate an effect a given number of times.
--
-- /Hint/: use 'replicate'.
--
-- >>> replicateA 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Just "hi")
-- Just ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Nothing
-- Nothing
--
-- >>> replicateA 3 ['a', 'b', 'c']
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = sequence . replicate n

-- | Ignores the second value, and puts the first value in a context
--
-- >>> 1 <$ Just 5
-- Just 1
-- >>> 3 <$ [1,2,3,4]
-- [3,3,3,3]
(<$) :: Functor f => a -> f b -> f a
(<$) = (<$>) . const

infixl 4 <$

-- | Apply the applicative effect of the second, using only the result of the first.
--
-- /Hint:/ Use liftA2 and type holes.....
--
-- >>> [1,2] <* [3,4,5]
-- [1,1,1,2,2,2]
--
-- >>> (Just 5) <* (Just 3)
-- Just 5
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const

infixl 4 <*

-- | Apply the applicative effect of the first, using only the result of the second.
--
-- /Hint:/ This seems familiar...
--
-- >>> [1,2,3] *> [3,4,5]
-- [3,4,5,3,4,5,3,4,5]
--
-- >>> (Just 5) *> (Just 3)
-- Just 3
(*>) :: Applicative f => f a -> f b -> f b
-- (*>) a b = (id <$ a) <*> b
(*>) = liftA2 (const id)

infixl 4 *>
