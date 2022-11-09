{-# LANGUAGE NoImplicitPrelude #-}
module ParserW8 where

import           Base
import           Functor
import           Applicative
import           ExercisesW8

import           Prelude                        ( reads )

-- | This is our Parser which holds a parsing function.
--   The function returns
--      - Nothing, if the parsing fails
--      - Just (r, p), where r is the unparsed portion of the input,
--        and p is the parsed input
newtype Parser a = Parser (String -> Maybe (String, a))

-- | Wrapper function for parsing
parse :: Parser a -> String -> Maybe (String, a)
parse (Parser p) inp = p inp

-- |
--
-- This may look weird, but we are using the instances of Functor you wrote previously.
--
-- Working from left (inner) -> right (outer):
--   The first <$> uses the function (-> r) instance for functor
--   The second <$> uses the Maybe instance for functor
--   The third <$> uses the tuple (, a) instance for functor
--
-- >>> parse (toUpper <$> char) "abc"
-- Just ("bc",'A')
instance Functor Parser where
    f <$> (Parser a) = Parser (((f <$>) <$>) <$> a)

    -- Can also be written more explicitly using a case statement
    -- f <$> (Parser a) = Parser $ \s -> case a s of
    --     Just (r, p) -> Just (r, f p)
    --     Nothing     -> Nothing

-- |
--
-- >>> parse (is '(' *> is 'a') "(a"
-- Just ("",'a')
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = Parser (\b -> Just (b, a))

    -- Pls don't do this in assignment.
    -- We will see how to handle this more gracefully in following weeks.
    -- (<*>) Parser (a -> b) -> Parser a -> Parser b
    (Parser f) <*> (Parser b) = Parser $ \i -> case f i of
        Just (r1, p1) -> case b r1 of
            Just (r2, p2) -> Just (r2, p1 p2)
            Nothing       -> Nothing
        Nothing -> Nothing

-- | Parse a single character
--
-- >>> parse char "abc"
-- Just ("bc",'a')
--
-- >>> parse char ""
-- Nothing
char :: Parser Char
char = Parser f
  where
    f ""       = Nothing
    f (x : xs) = Just (xs, x)

-- | Parse numbers as int until non-digit
--
-- >>> parse int "123abc"
-- Just ("abc",123)
--
-- >>> parse int "abc"
-- Nothing
int :: Parser Int
int = Parser $ \s -> case reads s of
    [(x, rest)] -> Just (rest, x)
    _           -> Nothing

-- | Parses a specific character, otherwise return Nothing
-- \Hint\: Use char and a `case` statment.
--
-- >>> parse (is 'c') "cba"
-- Just ("ba",'c')
-- >>> parse (is 'c') "abc"
-- Nothing
is :: Char -> Parser Char
is c = Parser $ \i -> case parse char i of
    Just (r1, x) -> if x == c then Just (r1, x) else Nothing
    Nothing      -> Nothing

-- | Parse a comma followed by an integer, ignoring the comma
--
-- /Hint/ Use *> or <* to ignore the result of the parser
--
-- >>> parse item ",1"
-- Just ("",1)
--
-- >>> parse item "1"
-- Nothing
item :: Parser Int
item = is ',' *> int

-- | Parse an inital character and an integer
--
-- >>> parse (open '(') "(1,2,3)"
-- Just (",2,3)",1)
--
-- >>> parse (open '[') "[1,2,3]"
-- Just (",2,3]",1)
--
-- >>> parse (open '[') "{1,2,3}"
-- Nothing
open :: Char -> Parser Int
open c = is c *> int

-- | Parse a tuple with two integers
-- /Hint/ Use open, item, and a variant of Lift
--
-- >>> parse parseIntTuple2 "(10,2)"
-- Just ("",(10,2))
--
-- >>> parse parseIntTuple2 "[10,2)"
-- Nothing
parseIntTuple2 :: Parser (Int, Int)
parseIntTuple2 = liftA2 (,) (open '(') (item <* is ')')
