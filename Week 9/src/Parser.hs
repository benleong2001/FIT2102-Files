{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import           Base
import           Functor
import           Applicative
import           Exercises
import           Traversable
import           Folds
import           ExercisesW8
import           ParserW8

import           Prelude                        ( reads )

{- ================================================================================= -}
{- ================================================================================= -}

-- | Parse a tuple with three integers
--
-- /Hint/: Same pattern as parseIntTuple2, now just with 3 things to parse ..
--
-- >>> parse parseIntTuple3 "(10,2,3)"
-- Just ("",(10,2,3))
--
-- >>> parse parseIntTuple3 "(1,2)"
-- Nothing
parseIntTuple3 :: Parser (Int, Int, Int)
parseIntTuple3 = liftA3 (,,) (open '(') (item <* is ',') (int <* is ')')
{- EXPLANATION: Basically the same as parseIntTuple2 from week 8.
                    But this time, we use item (aka open ',') for middle value. -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Repeat a parse some number of times
--
-- /Hint:/ Have a look at the types. I think we have seen something similar in ExercisesW8.hs..
--
-- /Hint 2/: We are /replicating/ the parser .. but wait it has an Applicative effect ..
--
-- >>> parse (thisMany 3 $ is 'a') "aaabc"
-- Just ("bc","aaa")
--
-- >>> parse (thisMany 4 $ is 'a') "aaabc"
-- Nothing
thisMany :: Int -> Parser a -> Parser [a]
thisMany = replicateA
{- EXPLANATION: thisMany does the same thing as replicateA, but specifically for Parser Applicative type. -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Parse a fixed length array of integers
--
-- /Hint/: thisMany runs a parser a fixed number of times
--
-- /Hint 2/: liftA2 can be used to apply a function across all the parse results
--
-- >>> parse (fixedArray 3) "[1,2,3]"
-- Just ("",[1,2,3])
--
-- >>> parse (fixedArray 4) "[1,2,3,10]"
-- Just ("",[1,2,3,10])
--
-- >>> parse (fixedArray 2) "[1,2,3]"
-- Nothing
fixedArray :: Int -> Parser [Int]
fixedArray x 
    | x == 0    = liftA2 (flip const) (is '[') (is ']' *> Parser (const $ Just ("", [])))
    | otherwise = liftA2 (:) (open '[') ((thisMany (x-1) item) <* is ']')
{- Probably the least neat function of the week :(
    Two scenarios:
        =-=-=-=-=-=-=-=-=-= SCENARIO 1 =-=-=-=-=-=-=-=-=-= 
        *Integer input = 0*
        In this case, we check if the first and second characters are '[' and ']' respectively
            If yes, return Just ("", []), else Nothing

        =-=-=-=-=-=-=-=-=-= SCENARIO 2 =-=-=-=-=-=-=-=-=-= 
        *Integer input > 0*
        We open the string and take the first integer since there should at least be 1 (if not, Nothing is returned)
            (open '[')
        Then for every subsequent int, we use item (x-1) times.
            (thisMany (x-1) item)
        Finally we 'close' off the array.
            (is ']')
    -}


{- ================================================================================= -}
{- ================================================================================= -}

-- | Write a function that parses the given string (fails otherwise).
--
-- /Hint/: Use traverse...
--
-- >>> parse (string "hello") "hello bob"
-- Just (" bob","hello")
-- >>> parse (string "hey") "hello bob"
-- Nothing
string :: String -> Parser String
string = traverse is
{- traverse takes in a function and a traversable value. In this case, 'is' is our function.
    traverse then takes in the input string as its traversable value. 
    This is so that it checks each character with the next input string
    
    e.g. (string "hello") "hello bob"
        --> (traverse is "hello") "hello bob"
        --> (Parser [is 'h', is 'e', is 'l', is 'l', is 'o']) "hello bob"
        --> apply is 'h': Just ("ello bob", 'h')
        --> apply is 'e': Just ("llo bob", 'he') and so on and so forth
    -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Return a parser that tries the first parser:
--
--   * if the first parser succeeds then use the first parser parser; or
--
--   * if the first parser fails, try the second parser.
--
-- For this specific question, deconstruct the Parser, but it shouldn't be
-- necessary for the other questions.
--
-- >>> parse (is 'a' ||| is 'c') "abc"
-- Just ("bc",'a')
--
-- >>> parse (is 'a' ||| is 'c') "cba"
-- Just ("ba",'c')
--
-- >>> parse (is 'a' ||| is 'c') "123"
-- Nothing
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = Parser $ \x -> case parse p1 x of
    Just y  -> Just y
    Nothing -> case parse p2 x of
        Just y  -> Just y
        Nothing -> Nothing
{- In short, read function header -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Parse a Nothing value. Parse the string "Nothing", if succeeds return Nothing
--
-- /Hint/: Use string and $>
--
-- >>> parse nothing "something"
-- Nothing
-- >>> parse nothing "Nothing"
-- Just ("",Nothing)
nothing :: Parser (Maybe a)
nothing = (string "Nothing") $> (Nothing)
{- Check if input string is "Nothing". 
    If yes, return Just ("", Nothing) else Nothing. what? -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Parse a Just value. Parse the string "Just ", followed by the given parser
--
-- >>> parse (just int) "Just 1"
-- Just ("",Just 1)
-- >>> parse (just int) "Nothing"
-- Nothing
just :: Parser a -> Parser (Maybe a)
just p = (*>) (string "Just ") $ (<$>) Just p
{- Check if the string starts with "Just ".
    If yes, proceed to check if the remaining can be parsed with input Parser. -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Parse a 'Maybe'
-- | This is named maybeParser due to a nameclash with Prelude.maybe
--
-- /Hint/: What does (|||) do?
--
-- /Hint 2/: Maybe types can only be just or nothing
--
-- >>> parse (maybeParser int) "Just 1"
-- Just ("",Just 1)
--
-- >>> parse (maybeParser int) "Nothing"
-- Just ("",Nothing)
--
-- >>> parse (maybeParser int) "Something Else"
-- Nothing
maybeParser :: Parser a -> Parser (Maybe a)
maybeParser = (|||) nothing . just 
{- Apply both just and nothing. just goes last since it needs an argument. -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Parse an array of length less then or equal to n
--
-- /Hint/: Use foldr, fixedArray and |||
--
-- /Hint 2/: atMostArray 2 = fixedArray 0 ||| fixedArray 1 ||| fixedArray 2
--
-- >>> parse (atMostArray 3) "[1,2,3,4,5]"
-- Nothing
-- >>> parse (atMostArray 10) "[1,2,3,4,5]"
-- Just ("",[1,2,3,4,5])
atMostArray :: Int -> Parser [Int]
atMostArray n = foldr (|||) (fixedArray n) (map fixedArray [0..n])
{- Apply fixed array n + 1 times, from 0 till n. It passes if any of it passes. Done using |||. -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Parse a sequence of arrays of length less then or equal to 3
--
-- /Hint/: Use traverse, parse and atMostArray
--
-- >>> validArrays ["[1,2]", "[1,2,3]", "[1]", "[]"]
-- Just [("",[1,2]),("",[1,2,3]),("",[1]),("",[])]
--
-- >>> validArrays ["[1,2]", "[1,2,3]", "[1]", "[]", "[1,2,3,4]"]
-- Nothing
validArrays :: [String] -> Maybe [(String, [Int])]
validArrays = traverse $ parse $ atMostArray 3
{- Execute the atMostArray parser with int 3. This will be the function for traverse. 
    The input list will be traverse's second argument.

    traverse is used because when using atMostArray 3 to the list items, we get:
        [Just ("", [..]), Just ("", [..]), Just ("", [..]), ..]
            but we want 
        Just [("", [..]), ("", [..]), ("", [..]), ..]
        -}

{- ================================================================================= -}
{- ================================================================================= -}

-- | Sum the values of a sequence of arrays of length less then or equal to 3
--
-- /Hint/: We want to /map/ a function (String, [Int]) -> Int into 2 layers of /nested/ Functors
-- validArrays :: [String] -> Maybe [(String, [Int])]
-- parseAndSum :: [String] -> Maybe [Int]
--
-- >>> parseAndSum ["[1,2]", "[1,2,3]", "[1]", "[]"]
-- Just [3,6,1,0]
--
-- >>> parseAndSum ["[1,2]", "[1,2,3]", "[1]", "[]", "[1,2,3,4]"]
-- Nothing
parseAndSum :: [String] -> Maybe [Int]
parseAndSum = (((snd . (sum <$>)) <$>) <$>) . validArrays
{- What a monstrosity. Let's break it down. 
parseAndSum = (((snd . (sum <$>)) <$>) <$>) . validArrays
    pas lst = ((snd . (sum <$>)) <$>) <$> (validArrays lst)
    pas lst = fmap ((snd . (sum <$>)) <$>) (validArrays lst)
    pas lst = fmap (map (snd . (sum <$>))) (validArrays lst)
    pas lst = fmap (map (snd . (fmap sum))) (validArrays lst)
    pas lst = fmap (map (\x -> snd(fmap sum x))) (validArrays lst)

validArrays lst returns a value with the format
    Maybe {                         uses fmap == <$>
        list [                      uses map  == <$>
            tuple (                 uses fmap == <$>
                String, [Int]       uses sum to sum up int in list
                )
            ]
        }

    What we want to is to access the int list, [Int], sum it up, and replace the tuple with the sum. 
    There are 3 layers which we need to peel off.
    Each layer can be accessed using fmap (or map for lists), also represented as <$>.
        When the layer is accessed, the new calculated values will override the previous contents of the layer.

    The most outer <$> is just to enter the Maybe layer, the second <$> is for the list layer.
        Note that the list layer opens up to an array of tuples, not just one.

    For each of the tuples, we map this function 'snd . (sum <$>)'
        sum <$>:
            This function fmaps the tuple with the sum function. 
            Since right-most item in the tuple is a (int) list, we can use sum to sum the int list
            This will replace the second list with the sum, but doesn't replace the tuple.
            tuple(String, [Int]) --> tuple(String, summed_integers)
        
        snd:
            We want to replace every tuple with just the sum. 
            Therefore, we use the snd function to get the second element, in this case the sum,
                and then replace the tuple with this element.
            tuple(String, summed_integers) --> summed_integers.
-}