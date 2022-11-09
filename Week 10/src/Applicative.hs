{-# LANGUAGE NoImplicitPrelude, InstanceSigs #-}
-- | Implement instances for the 'Applicative' typeclass.
module Applicative
  ( Applicative
  , pure
  , (<*>)
  , nestedApply
  ) where

import           Base
import           Functor                 hiding ( (<$>) )
import           Prelude                        ( (++)
                                                , map
                                                )


class Functor f => Applicative f where
  pure :: a -> f a
  -- Pronounced "apply"
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

-- | The 'Applicative' class derives from 'Functor', rewrite `fmap` using only
-- `pure` and `<*>`.
--
-- Be cautious when using this `<$>` when defining your own `<*>`s - as this
-- calls `<*>`, you may get an infinite loop!
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nothing
-- Nothing
--
-- >>> (+1) <$> [1, 2, 3]
-- [2,3,4]
(<$>) :: Applicative f => (a -> b) -> f a -> f b
(<$>) f a = pure f <*> a

-- | Insert into Id.
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Applicative Id where
  pure :: a -> Id a
  pure = Id

  (<*>) :: Id (a -> b) -> Id a -> Id b
  (<*>) (Id f) (Id v) = Id (f v)

-- | Apply a list of functions over a list of elements, producing the
-- concatenation of the successive results.
--
-- /Hint/ Use foldr OR list comprehension
--
-- >>> [(+1), (*2)] <*> [1, 2, 3]
-- [2,3,4,2,4,6]
instance Applicative [] where
  pure :: a -> [a]
  pure x = [x]

  (<*>) :: [a -> b] -> [a] -> [b]
  (<*>) fs xs = [ f x | f <- fs, x <- xs ]
    -- (<*>) fs ls = foldr ((++) . flip map ls) [] fs
    -- (<*>) (f : fs) l = map f l ++ (fs <*> l)
    -- (<*>) []       _ = []

-- | Apply to a Maybe, must return `Nothing` if either the function or the
-- element is a `Nothing`.
--
-- >>> Just (+8) <*> Just 7
-- Just 15
--
-- >>> Nothing <*> Just 7
-- Nothing
--
-- >>> Just (+8) <*> Nothing
-- Nothing
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) (Just f) (Just v) = Just (f v)
  (<*>) _        _        = Nothing

-- | Applicative instance of a function that takes an r.
-- /Hint/: `pure x` creates a function that always return x, no matter what it's given.
--
-- >>> f = pure "hello"
-- >>> f 95
-- "hello"
--
-- /Hint/: `(<*>)` takes a function which produces a function for a given r,
--         and a function which produces a value for a given r,
--         and gives a function that applies:
--             the function produced by the first argument,
--             to the value produced by the second argument.
--
-- >>> import Data.Tuple
-- >>> f = (++) <$> fst <*> snd
-- >>> f ("a","b")
-- "ab"
--
-- >>> conditionalPrepend x b = if b then (x:) else id
-- >>> (conditionalPrepend<*>) (<5) 1 [2,3]
-- [1,2,3]
instance Applicative ((->) r) where
  pure :: a -> (r -> a)
  pure a _ = a
  -- OR
  -- pure a = (\_ -> a)
  -- pure = const

  (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
  (<*>) f x a = f a $ x a
  -- OR
  -- (<*>) f g = \x -> (f x) (g x)

-- | Takes an of functions in an Applicative context, and a value in the same context
--   and applies the functions to the value
--
-- >>> nestedApply [Just (+1), Just (*2)] (Just 2)
-- [Just 3,Just 4]
--
-- >>> nestedApply [Id (++" hello"),Id (++" world!")] (Id "hello")
-- [Id "hello hello",Id "hello world!"]
nestedApply
  :: (Applicative f, Applicative g) => g (f (a -> b)) -> f a -> g (f b)
nestedApply fs a = (<*> a) <$> fs

{-
    ******************** OPTIONAL **************************8
-}
-- | Apply to a RoseTree, i.e. return a tree composed of trees created by the
-- successive application of functions to initial nodes.
--
-- /Hint/: complete 'nestedMap' and the 'Functor' instance for 'RoseTree' (in 'Functor.hs')
-- first.
-- You will need to replace `>` with `>>>` to get test cases to run.
--
-- /Hint/: study the tests below closely...
-- You will see that for:
--   lhs <*> rhs
-- we create a new rose tree whose root is the
-- application of the function at the root of lhs to the root of rhs,
-- then its children are the mapping of the root function, mapped over the children of rhs,
-- and concatenated with the application of the remaining functions in lhs to rhs.
--
-- /Hint/: use 'nestedMap' and 'nestedApply'
--
-- > (Node (+1) [Node (*2) []]) <*> Nil
-- Nil
--
-- > Nil <*> (Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]])
-- Nil
--
-- > (Node (+1) []) <*> (Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]])
-- Node 8 [Node 2 [],Node 3 [],Node 4 [Node 5 []]]
--
-- > (Node (+1) [Node (*2) []]) <*> (Node 5 [Node 2 [], Node 8 [Node 1 []]])
-- Node 6 [Node 3 [],Node 9 [Node 2 []],Node 10 [Node 4 [],Node 16 [Node 2 []]]]
instance Applicative RoseTree where
  pure :: a -> RoseTree a
  pure x = Node x []

  (<*>) :: RoseTree (a -> b) -> RoseTree a -> RoseTree b
  (<*>) (Node f fs) r@(Node v vs) =
    Node (f v) (nestedMap f vs ++ nestedApply fs r)
  (<*>) _ _ = Nil
