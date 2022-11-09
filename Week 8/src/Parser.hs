{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import           Base
import           Functor
import           Applicative
import           Exercises

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

{- =================================================================================
    =-=-=-=-=--= EXPLANATION FOR <* AND *> =-=-=-=-=--=
    For <*
    - Refer to this as left apply, implying to IGNORE right.
    - Used in the format of 'param1 <* param2'
    - In the code below, param1 and 2 are both parsers. 
        - Since parsers need an input, <* will also take some input
    - Functionality:
        - Let's say we have this code --> (parser1 <* parser2) input
        - The parser1 will operate on input first, returning 
            either Nothing or Just (remain, value)
        - Then parser2 will operate on remain, returning
            either Nothing or Just (new_remain, value)
        - Note that value remains the same because we IGNORE right
        - E.g. char <* int "a2cd"
            char removes a --> Just ("2cd", "a")
            int removes 2 from remain --> Just ("cd", "a")

    For *>
    - Basically the same logic, but now we ignore the left value
    - Using the same example:
        - E.g., char *> int "a2cd"
            char removes a --> Just ("2cd", "a")
            int removes 2 from remain, and produces value of 2
                --> Just ("cd", 2) (NOTE: the value is an int since we used the int parser)

    NOTE:
        For the functions below, when the input is not suitable,
            the function simply returns Nothing
-}

{- ================================================================================= -}
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
{- Brief explanation: Use is to remove the comma, 
                        leaving the int as a string
                        then use int to take out the integer -}

{- ================================================================================= -}
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
open chr = is chr *> int
{- Brief explanation: Use is to remove the character chr, 
                        leaving the container 'open' 
                        then use int to take out the first integer -}

{- ================================================================================= -}
-- | Parse a tuple with two integers
--
-- /Hint/ Use open, item, and a variant of lift
--
-- >>> parse parseIntTuple2 "(10,2)"
-- Just ("",(10,2))
--
-- >>> parse parseIntTuple2 "[10,2)"
-- Nothing
parseIntTuple2 :: Parser (Int, Int)
parseIntTuple2 = liftA2 (,) (open '(') (item <* is ')')

{- 
EXPLANATION:
liftA2 f = (<*>) . liftA f = (<*>) . (<$>) f        [This is all from previous files]
liftA2 f x y = (<*>) ((<$>) f x) y
= (<*>) (f <$> x) y
= (f <$> x) <*> y
= f <$> x <*> y

Our inputs for liftA2 will be 
    (,)
        This is like a tuple constructor
        Uses the values produced by the parsers to form the tuple

    (open '(')
        To 'open' up our container (if it is one)
        value = the first int in the container

    (item <* is ')')
        for item, it takes in ",x" where x is some int
             returns Just ("", x)
             value = x
        for is ')', it takes in ')...'
             returns Just('...', ')')
             value = doesn't matter :(

Logic:
    The input should be some tuple (as a string) for this to work.
    The code opens the tuple first and takes the first int [Using open]
    Then we pass the comma, directly accessing the next int [Using item]
    However, we only take the next int if immediately after the int is a closing round bracket [Using is]
    Using the two integers here, we apply the tuple constructor and return this tuple

    There are 3 parsing here; open, item and is. By the end of all 3 parsing, the remaining string will just be ""
    So, the function returns 
        Just (remain, tuple) = Just ("", (fst, snd))
        where fst = first int
              snd = second int
-}