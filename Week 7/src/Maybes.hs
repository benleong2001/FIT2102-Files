-- | Re-implementation of 'Maybe'-related functions.
module Maybes where

import Data.Maybe(fromJust)

-- $setup
-- >>> ljust = [Just 1, Just 7, Just 3]
-- >>> lnaught = [Just 3, Nothing, Just 8]

-- | Simple boolean check for 'Maybe' values.
--
-- >>> isJust (Just 1)
-- True
--
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- | Inverse of 'isJust'.
--
-- >>> isNothing (Just 1)
-- False
--
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- | Extract the value of a 'Just' but return a fallback in case of 'Nothing'.
--
-- >>> fromMaybe (Just 3) 7
-- 3
--
-- >>> fromMaybe Nothing 7
-- 7
fromMaybe :: Maybe a -> a -> a
fromMaybe m a = if (isJust m) 
    then (fromJust m) 
    else a

-- | Gather 'Just' values in a list, filter the 'Nothing'.
--
-- >>> catMaybe ljust
-- [1,7,3]
--
-- >>> catMaybe lnaught
-- [3,8]

-- [Just 1, Just 7, Just 3]
-- foldl [], Just 1 -> ++ [] 
catMaybe :: [Maybe a] -> [a]
catMaybe = map (fromJust) . filter (isJust)
