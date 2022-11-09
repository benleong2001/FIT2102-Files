
{-# LANGUAGE NoImplicitPrelude, InstanceSigs #-}
-- | Implement instances and convenience functions for the 'Monad' typeclass.
module Exercises where

import           Applicative             hiding ( (<*>) )
import           Base                    hiding ( (>>) )
import           Data.Foldable                  ( Foldable
                                                , foldl
                                                , foldr
                                                )
import           Functor
import           Monad
import           Traversable                    ( Traversable
                                                , traverse
                                                )

-- | Flatten a combined structure to a single structure.
--
-- /Hint/: Use (=<<) and pass a very, very simple function in as the first argument.
--
-- /Hint 2/: Check your types! Either through static analysis or type holes
--
-- List Monad: join concatenates
-- >>> join [[1, 2, 3], [1, 2]]
-- [1,2,3,1,2]
--
-- Maybe Monad: join flattens nested maybes
-- >>> join (Just Nothing)
-- Nothing
--
-- >>> join (Just (Just 7))
-- Just 7
--
-- >>> (join (+)) 3
-- 6
join :: Monad f => f (f a) -> f a
join = ((=<<) id)

-- | Implement a flipped version of '=<<'.
--
-- /Optional/: Do this using 'join' and '<$>'
--
-- Divide an integer by two if it is even.
--
-- >>> :{
-- half x | even x    = Just (x `div` 2)
--        | otherwise = Nothing
-- :}
--
-- >>> Just 20 >>= half
-- Just 10
--
-- >>> Just 20 >>= half >>= half >>= half
-- Nothing
(>>=) :: Monad f => f a -> (a -> f b) -> f b
(>>=) x = join . flip (<$>) x

-- | Sequentially call two monadic actions
--
-- >>> [1,2,3] >> [2,3,4]
-- [2,3,4,2,3,4,2,3,4]
(>>) :: Monad m => m a -> m b -> m b
(>>) x = (>>=) x . const 

-- | 'return' is just 'pure', which is available because a Monad is also an Applicative
return :: (Monad m) => a -> m a
return = pure

-- | map a function with a monadic effect across a Traversable.
--
-- /Hint/: this is kind of a trick question, look closely at the type of mapM
-- and look at the type of the function that we defined for Traversable in Week 9 -
-- keeping in mind that a Monad is also Applicative.  Note that we are also
-- importing the solution to Traversable.hs from Week9.
--
-- /Hint/: What does traverse do?
--
-- >>> doubleSmallNumbers n = if n < 3 then Just (n+n) else Nothing
-- >>> mapM doubleSmallNumbers [1,2,3,4]
-- Nothing
--
-- >>> mapM doubleSmallNumbers [1,2,1,2]
-- Just [2,4,2,4]
mapM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapM = traverse

-- | ------------------------------------------------------
-- | -------------------- Supplementary -------------------
-- | ------------------------------------------------------
-- | Uncomment the test cases to run them!

-- | The following are useful functions that demonstrate excellent understanding of Monads

-- | Map a function with a monadic effect across a Foldable - disregard the results but keep the effect.
--
-- /Hint/: use `foldr`.
--
-- /Hint 2/: Note that we are returning unit `()` in the Monad.  You'll need to put a `()` into the Monad as the initial value for the foldr.
--
-- /Hint 3/: Use `>>` for the sequencing.
--
-- >>> doubleSmallNumbers n = if n < 3 then Just (n+n) else Nothing
-- >>> mapM_ doubleSmallNumbers [1,2,3,4]
-- Nothing
-- 
-- >>> mapM_ doubleSmallNumbers [1,2,1,2]
-- Just ()
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = foldr ((>>) . f) (pure ())

-- | Fold a list using a monadic function.
--
-- /Tip/: Fold the monad from left to right on the list of arguments.
--
-- /Hint/: In the base case, use pure to put the accumulator into the context
--         In the general case, you will use bind to apply the specified function
--
-- Example: Sum the elements in a list, but fail if any is greater than 10.
-- 
-- >>> :{
-- small acc x
--   | x < 10 = Just (acc + x)
--   | otherwise = Nothing
-- :}
-- 
-- >>> foldM small 0 [1..9]
-- Just 45
-- 
-- >>> foldM small 0 [1..100]
-- Nothing
foldM :: Monad f => (b -> a -> f b) -> b -> [a] -> f b
foldM f ac ar = foldl fun (pure ac) ar
    where
        fun ac x = ac >>= (flip f x)

-- | Implement composition within the 'Monad' environment. Called: "Kleisli
-- composition."
--
-- /Hint/: Apply one of the two functions directly, and then use (>>=) or (=<<).
--
-- /Note/: If you use this in a useful way in your assignment, we will be very impressed
--
-- >>> (\n -> [n, n]) <=< (\n -> [n + 1, n + 2]) $ 1
-- [2,2,3,3]
(<=<) :: Monad f => (b -> f c) -> (a -> f b) -> a -> f c
(<=<) f g x = (g x) >>= f
infixr 1 <=<

-- | The following highlight the relationship between Functor, Applicative, and Monad.
-- | It is a nice exercise, but may not be that practically useful.

-- | Rewrite /fmap/ using Monads (i.e. use bind)
--
-- >>> (+1) <$> [1, 2, 3]
-- [2,3,4]
-- 
-- >>> (+1) <$> Nothing
-- Nothing
fmap :: Monad f => (a -> b) -> f a -> f b
fmap f = (=<<) (pure . f)

-- | Rewrite /apply/ '(<*>)' using /bind/ @(=<<)@ and /fmap/ '(<$>)'.
--
-- /Hint/: Look closely at the type of (=<<) above.  In the expression 'f =<< x':
--                x is a thing in the context,
--                f is a function that is applied to thing (after it is taken out of the context)
--         The first argument to <*> is a function of type f (a -> b), i.e. a function inside the context.
--         The second argument to <*> is a thing in the context.
--         You can use =<< to get the function out of the context.
--         The first argument to =<< needs to be a function that puts its result in the context (a -> f b).
-- >>> Just (+10) <*> Just 8
-- Just 18
(<*>) :: Monad f => f (a -> b) -> f a -> f b
(<*>) f x = (flip (<$>) x) =<< f
infixl 4 <*>
