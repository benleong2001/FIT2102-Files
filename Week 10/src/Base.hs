{-# OPTIONS_HADDOCK hide #-}
-- | Restricted Prelude
module Base(
  module Data.Monoid,
  module Data.Maybe,
  module Data.Bool,
  module Data.Char,
  module Data.Eq,
  module System.IO,
  RoseTree(..), Id(..), Pair(..),
  Num(..), Int, Show(..), Integer, String, Ord(..), RealFrac,
  flip, foldr, even, undefined, const, id, reverse, foldl, succ,
  last, lines, error, div, replicate, zipWith, elem, notElem,
  (.), ($), (>>)
) where

import Prelude(
  Num(..), Int, Show(..), Integer, String, Ord(..), RealFrac,
  flip, foldr, even, undefined, const, id, reverse, foldl, succ,
  last, lines, error, div, replicate, zipWith, elem, notElem,
  (.), ($), (>>))

import Data.Monoid
import Data.Maybe
import Data.Bool
import Data.Char
import Data.Eq
import System.IO

-- | A 'RoseTree' is a tree with an arbitrary number of children per node.
data RoseTree a = Node a [RoseTree a] | Nil
  deriving(Show)

-- | The 'Id' type wraps a value in the identity.
data Id a = Id a
  deriving(Show)

-- | A 'Pair' contains two values.
data Pair a = Pair a a
  deriving(Show)
