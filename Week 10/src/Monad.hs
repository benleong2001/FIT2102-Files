{-# LANGUAGE NoImplicitPrelude, InstanceSigs #-}
-- | Implement instances and convenience functions for the 'Monad' typeclass.
module Monad
  ( Monad
  , (=<<)
  ) where

import           Applicative
import           Base
import           Functor
import           Prelude                        ( (++) )

-- | All instances of the 'Monad' typeclass must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--
-- > g =<< (f =<< x) = ((g =<<) . f) =<< x
class Applicative f => Monad f where
  -- Pronounced: "bind."
  (=<<) :: (a -> f b) -> f a -> f b
infixr 1 =<<

-- | Bind a function on a list.
--
-- /Hint/: remember lists are Monoids, i.e. mempty is [], mappend concatenates two lists, mconcat flattens a list of lists.
-- /Hint/: if the parameters to (=<<) are f and l, what does mapping f over l give you?
--
-- >>> (\n -> [n, n]) =<< [1, 2, 3]
-- [1,1,2,2,3,3]
instance Monad [] where
  (=<<) :: (a -> [b]) -> [a] -> [b]
  (=<<) f = mconcat . (<$>) f

-- | Bind a function on a 'Maybe'.
-- /Hint/: Use pattern matching to handle the Nothing and the Just cases.
--
-- >>> justDouble n = Just (n + n)
-- >>> justDouble =<< Just 7
-- Just 14
--
-- >>> justDouble =<< Nothing
-- Nothing
instance Monad Maybe where
  (=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b
  (=<<) f (Just x) = f x
  (=<<) _ Nothing = Nothing

-- | ------------------------------------------------------
-- | -------------------- Supplementary -------------------
-- | ------------------------------------------------------

-- | Implement bind for functions.
--
-- /Hint/: Return a lambda function and follow the types!
--
-- The function Monad context allows us to sequence or compose functions together.
-- The right-to-left bind (=<<) takes
--   - a binary function f and a unary function g, and
--   - creates a new unary function
-- The new function will
--   - apply g to its argument, then
--   - apply f to the result and the original argument
--
-- OK, that might seem a bit esoteric, but it lets us achieve some nifty things.
--
-- For example, below we compute `(3*2) + 3`, but we did it by using the argument `3`
-- to two different functions without explicitly passing it to either!
--
-- You can imagine a situation where you need to chain together a bunch of functions, but
-- they all take a common parameter, e.g. a file name or common contextual information.
--
-- -- >>> ((+) =<< (*2)) 3
-- 9
--
-- In the next example we use the 3 in three functions without passing it directly to any of them:
-- -- >>> ((*) =<< (+) =<< (*2)) 3
-- 27
--
-- -- >>> ((++) =<< ((+1)<$>)) [1,2,3]
-- [2,3,4,1,2,3]
instance Monad ((->) r) where
  (=<<) :: (a -> (r -> b)) -> (r -> a) -> (r -> b)
  (=<<) f g r = f (g r) r
