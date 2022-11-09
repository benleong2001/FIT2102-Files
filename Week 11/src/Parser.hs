-- | Implementation of a parser-combinator.
module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char                      ( isAlpha
                                                , isDigit
                                                , isLower
                                                , isSpace
                                                , isUpper
                                                )
import           Instances

-- | -------------------------------------------------
-- | --------------- Core parsers --------------------
-- | -------------------------------------------------

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse (failed UnexpectedEof) "abc")
-- True
failed :: ParseError -> Parser a
failed = P . const . Error

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = P . const . Error . UnexpectedChar

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
--
-- >>> parse char "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse char "")
-- True
char :: Parser Char
char = P f
 where
  f ""       = Error UnexpectedEof
  f (x : xs) = Result xs x

-- | Parse numbers as int until non-digit
--
---- >>> parse int "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse int "")
-- True
--
-- >>> isErrorResult (parse int "a")
-- True
int :: Parser Int
int = P f
 where
  -- This is okay because the case statement is small
  f "" = Error UnexpectedEof
  f x  = case readInt x of
    Just (v, rest) -> Result rest v
    Nothing        -> Error $ UnexpectedChar (head x)

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
eof :: Parser ()
eof = P f
 where
  f "" = Result "" ()
  f x  = Error $ ExpectedEof x

-- | Return a parser that tries the first parser for a successful value, then:
--
--   * if the first parser succeeds then use this parser; or
--
--   * if the first parser fails, try the second parser.
--
-- >>> parse (char ||| pure 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed UnexpectedEof ||| pure 'v') ""
-- Result >< 'v'
--
-- >>> parse (char ||| pure 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed UnexpectedEof ||| pure 'v') "abc"
-- Result >abc< 'v'
(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 = P $ \x -> case parse p1 x of
  Error _ -> parse p2 x
  _       -> parse p1 x
infixl 3 |||

-- | `chain p op` parses 1 or more instances of `p` separated by `op`
-- | (see chainl1 from Text.Parsec)
-- | This can be a very useful parser combinator
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      ||| pure a

-- | -------------------------------------------------
-- | --------------- Satisfy parsers -----------------
-- | -------------------------------------------------
-- | All of these parsers use the `satisfy` parser!
-- | /Hint/: The solutions are all very short

-- | Return a parser that produces a character but fails if:
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = char >>= f'
 where
  -- This is okay because guards are small
  f' c | f c       = pure c
       | otherwise = unexpectedCharParser c


-- | Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
--
-- >>> parse (is 'c') "c"
-- Result >< 'c'
--
-- >>> isErrorResult (parse (is 'c') "")
-- True
--
-- >>> isErrorResult (parse (is 'c') "b")
-- True
is :: Char -> Parser Char
is = satisfy . (==)


-- | Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
--
-- >>> parse (isNot 'c') "b"
-- Result >< 'b'
--
-- >>> isErrorResult (parse (isNot 'c') "")
-- True
--
-- >>> isErrorResult (parse (isNot 'c') "c")
-- True
isNot :: Char -> Parser Char
isNot = satisfy . (/=)


-- | Write a function that parses one of the characters in the given string.
--
-- /Hint/: What does `elem` do? What are its parameters?
--
-- >>> parse (oneof "abc") "bcdef"
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof "abc") "def")
-- True
oneof :: String -> Parser Char
oneof = satisfy . (flip elem)


-- | Write a function that parses any character, but fails if it is in the
-- given string.
--
-- /Hint/: What does `notElem` do? What are its parameters?
--
-- >>> parse (noneof "bcd") "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof "abcd") "abc")
-- True
noneof :: String -> Parser Char
noneof = satisfy . (flip notElem)


-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a digit.
--
-- /Hint/: Use the 'isDigit' function
digit :: Parser Char
digit = satisfy isDigit


-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
--
-- /Hint/: Use the 'isSpace' function
space :: Parser Char
space = satisfy isSpace


-- | Return a parser that produces a lower-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not lower-case.
--
-- /Hint/: Use the 'isLower' function
lower :: Parser Char
lower = satisfy isLower


-- | Return a parser that produces an upper-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not upper-case.
--
-- /Hint/: Use the 'isUpper' function
upper :: Parser Char
upper = satisfy isUpper


-- | Return a parser that produces an alpha character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not alpha.
--
-- /Hint/: Use the 'isAlpha' function
alpha :: Parser Char
alpha = satisfy isAlpha


-- | -------------------------------------------------
-- | --------------- Token parsers -------------------
-- | -------------------------------------------------

-- | Write a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- /Hint/: You can use the Monad instance or Applicatives
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok :: Parser a -> Parser a
tok = flip (<*) spaces
-- tok p = p <* spaces
{-  EXPLANATION
      Execute input parser
      Then remove any trailing spaces while keeping the parser's result(s)  -}


-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (charTok 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') "dabc")
-- True
charTok :: Char -> Parser Char
charTok = tok . is 


-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- /Hint/: We just implemented `charTok`
--
-- >>> parse commaTok ",123"
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok "1,23")
-- True
commaTok :: Parser Char
commaTok = charTok ','


-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok = tok . string


-- | -------------------------------------------------
-- | ------------- Sequential parsers ----------------
-- | -------------------------------------------------

-- | Return a parser that continues producing a list of values from the given
-- parser.
--
-- /Hint/: Use 'list1'
--
-- /Hint 2/: We want to return a parser that evaluates to [] if there are no valid values
--
-- >>> parse (list char) ""
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list char) "abc"
-- Result >< "abc"
--
-- >>> parse (list (char *> pure 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (char *> pure 'v')) ""
-- Result >< ""
list :: Parser a -> Parser [a]
list = flip (|||) (pure []) . list1
-- list p = list1 ||| pure []
{-  EXPLANATION:
      Call list1 using the same parser input and if it fails, simply return empty list  -}


-- | Return a parser that produces at least one value from the given parser
-- then continues producing a list of values from the given parser (to
-- ultimately produce a non-empty list).
--
-- /Hint/: Use `list`
--
-- >>> parse (list1 (char)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (char *> pure 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (char *> pure 'v')) "")
-- True
list1 :: Parser a -> Parser [a]
list1 = flip chain (pure (++)) . (=<<) (pure . (:[]))
-- list1 p = chain (p >>= (pure . (:[]))) (pure (++))
{-  EXPLANATION
      Use input parser multiple times using chain.
          Noting that the parser is changed so that the resulting value is a list
          i.e., Parser a -> Parser [a]  
      Connect the results using (++) function wrapped in a Parser using pure.

      chain is used because it only works if there is at least 1 char to parse  -}


-- | Write a parser that will parse zero or more spaces.
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces " abc"
-- Result >abc< " "
--
-- >>> parse spaces "abc"
-- Result >abc< ""
spaces :: Parser String
spaces = list space
{-  EXPLANATION
      Use list to parse 0 or more spaces  -}


-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces1 " abc"
-- Result >abc< " "
--
-- >>> isErrorResult $ parse spaces1 "abc"
-- True
spaces1 :: Parser String
spaces1 = list1 space
{-  EXPLANATION
      Use list1 to parse 1 or more spaces   -}

-- | Return a parser that produces the given number of values off the given
-- parser.  This parser fails if the given parser fails in the attempt to
-- produce the given number of values.
--
-- /Hint/: You can use 'sequence' and 'replicate'.
--
-- /Hint 2/: We are /replicating/ the parser .. but wait it has a Monadic effect ..
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany :: Int -> Parser a -> Parser [a]
thisMany n p = sequence (replicate n p)

-- | Write a function that parses the given string (fails otherwise).
--
-- /Hint/: Use 'is' and 'traverse'.
--
-- >>> parse (string "abc") "abcdef"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string "abc") "bcdef")
-- True
string :: String -> Parser String
string = traverse is

-- | Write a function that produces a non-empty list of values from repeating the
-- given parser (which must succeed at least once), separated by the second
-- given parser.
--
-- /Hint/: You can use the Monad instance, using `list` to repeat a parser
--        and (>>) to ignore values
--
-- /Hint 2/: You can also use the Applicative instance with `liftA2`
--
-- /Hint 3/: Try breaking this parser down into parts! What does this need to do?
-- - Run the first given parser to get a value, then
-- - Repeat a parser that
--   - runs the second given parser (separator parser), ignoring its result, and
--   - runs the first given parser, then produces its result
-- - Combine all the results into a list
--
-- >>> parse (sepby1 char (is ',')) "a"
-- Result >< "a"
--
-- >>> parse (sepby1 char (is ',')) "a,b,c"
-- Result >< "abc"
--
-- >>> parse (sepby1 char (is ',')) "a,b,c,,def"
-- Result >def< "abc,"
--
-- >>> isErrorResult (parse (sepby1 char (is ',')) "")
-- True
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p = liftA2 (:) p . list . flip (>>) p
-- sepby1 p s = liftA2 (:) p $ list $ s >> p
{-  EXPLANATION
      First input parser - to parse value
      Second input parser - to parse separator

      Use first input parser once first.
      Then create a new parser where:
        The separator is parsed first,
        then the parse the value.
      Repeatedly carry out this parser using list   -}


-- | Write a function that produces a list of values from repeating the given
-- parser, separated by the second given parser.
--
-- /Hint/: If `sepby1` fails, that means no values were parsed
--
-- >>> parse (sepby char (is ',')) ""
-- Result >< ""
--
-- >>> parse (sepby char (is ',')) "a"
-- Result >< "a"
--
-- >>> parse (sepby char (is ',')) "a,b,c"
-- Result >< "abc"
--
-- >>> parse (sepby char (is ',')) "a,b,c,,def"
-- Result >def< "abc,"
sepby :: Parser a -> Parser s -> Parser [a]
sepby p = flip (|||) (pure []) . sepby1 p
-- sepby p s = sepby1 p s ||| pure []
{-  EXPLANATION
      Call sepby1. If it fails, return empty list   -}


-- | Write a function that produces a parser for an array (list)
--
-- /Hint/: This uses a similar idea to all the other parsers in this section
--
-- /Hint 2/: This parser can be written in 3 parts:
-- (1) parse the open bracket
-- (2) parse the array contents
-- (3) parse the closing bracket
-- Then return the result of (2)
--
-- /Hint 3/: Array contents are either empty (empty array "[]"), or comma separated values.
-- Do we have something that repeats a parser, separated by some other parser?
--
-- >>> parse (array $ tok int) "[1,2,3,4,5]"
-- Result >< [1,2,3,4,5]
--
-- >>> parse (array $ tok int) "[ 1,2,3,4,5]"
-- Result >< [1,2,3,4,5]
--
-- >>> parse (array $ tok int) "[]"
-- Result >< []
--
-- >>> parse (array $ tok int) "[ ] "
-- Result >< []
--
-- >>> isErrorResult (parse (array $ tok int) "1")
-- True
array :: Parser a -> Parser [a]
array = (*>) (charTok '[') . flip (<*) (charTok ']') . flip sepby commaTok
-- array p = charTok '[' *> sepby p commaTok <* charTok ']'
{-  EXPLANATION
      Parse the '[' char to "open" up the input string.
      Use sepby to retrieve the characters using the input parser.
      Parse the ']' char to "close" up the input string.
-}