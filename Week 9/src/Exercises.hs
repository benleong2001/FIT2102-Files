{-# LANGUAGE NoImplicitPrelude #-}
module Exercises where

import           Base hiding (maybe)
import           Functor
import           ExercisesW8

-- | Ignores the first value, and puts the second value in a context
--
-- >>> Just 5 $> 1
-- Just 1
-- >>> [1,2,3,4] $> 3
-- [3,3,3,3]
($>) :: Functor f => f b -> a -> f a
($>) j = flip (<$>) j . const

{- $> takes in an Applicative value, f b, then a normal value, a

    Recap: <$> maps a function onto the Applicative value. (func) <$> (Applicative value) = Applicative (func value)
    Since flipped is used, we input the Applicative value first.
    As for the function, we use const here to take in a so that it will always return a when called.

    So, when we map 'const a' onto the Applicative, f b, every value in b becomes a.
    -}


infix 4 $>
