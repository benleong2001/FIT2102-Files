module HelperFile where

import Control.Applicative
import Data.Builder
import Data.Char
import Parser
import Prelude

{-  BNF for Lambda Expressions (Just for reference)
   <params>      ::= <lambda> 1*<alpha> <dot>
   <param>       ::= <lambda> <alpha> <dot>
   <lambdaExpr>  ::= 1*<longForm> | 1*<shortForm>

   <longBase>    ::= <openBrac> <param> 1*<longBody> <closeBrac>
   <longBody>    ::= 1*<longInner>
   <longInner>   ::= <longLambda> | <alpha> | <openBrac> <longBody> <closeBrac>
   <longLambda>  ::= 1*longBase | <openBrac> <longLambda> <closeBrac>

   <shortBase>   ::= <openBrac> <shortBase> <closeBrac> | <params> 1*<shortBody>
   <shortBody>   ::= 1*<shortInner>
   <shortInner>  ::= <shortLambda> | <alpha> | <openBrac> <shortBody> <closeBrac>
   <shortLambda> ::= 1*<shortBase> | <openBrac> <shortLambda> <closeBrac>

   <lambda>      ::= 'λ'   <spaces>
   <alpha>       ::= [a-z] <spaces>
   <dot>         ::= '.'   <spaces>
   <openBrac>    ::= '('   <spaces>
   <closeBrac>   ::= ')'   <spaces>
   <spaces>      ::= ' '   <spaces> | ''
-}

{- Extra Data Types and Types -}
type Chainer = Builder -> Builder -> Builder
type Params = Builder -> Builder
type ChainL = Logic -> Logic -> Logic
type ChainM = Math -> Math -> Math

{- Helper Functions -}
-- isErrorResult detects if the ParseResult value is an error
-- Used for test case(s) only
-- This function is from Tutorial 11
isErrorResult :: ParseResult a -> Bool
isErrorResult (Error _) = True
isErrorResult _         = False

-- chain function from Tutorial 11
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

-- NOTE: Some of the functions below are from previous Tutorials
--        but they are all made by me. 
-- Parses alphabets (a to z)
alpha :: Parser Char
alpha = satisfy isAlpha

-- Parses at least 1 space
spaces1 :: Parser String
spaces1 = space >> spaces

-- Parses the input string then 0 or more spaces
stringTok :: String -> Parser String
stringTok = flip (<*) spaces . string

-- Parses the input character then 0 or more spaces
isTok :: Char -> Parser Char
isTok = flip (<*) spaces . is

-- Parses the characters ( and ) then 0 or more spaces
openBrac, closeBrac :: Parser Char
openBrac  = isTok '('
closeBrac = isTok ')'

betwBrac :: Parser a -> Parser a
betwBrac = between openBrac closeBrac

strParse :: String -> Parser a -> Parser a
strParse = (>>) . stringTok 

lamify :: String -> Params
lamify = foldl1 (.) . (<$>) lam

termify :: String -> Builder
termify = foldl1 ap . (<$>) term

apB :: Builder -> Chainer
apB builder b1 b2 = builder `ap` b1 `ap` b2 

{- Part 1 -}
lambdaSym, dot :: Parser Char
lambdaSym = isTok 'λ'
dot       = isTok '.'

inParam :: Parser a -> Parser a
inParam = between lambdaSym dot

param, params :: Parser Params
param  = inParam (lam <$> alpha)
params = inParam (lamify <$> list1 alpha)

{- Part 2 -}
data Logic = InLgc Logic
          | Not  Logic 
          | And  Logic Logic 
          | Or   Logic Logic 
          | If   Logic Logic Logic 
          | Bool Builder
          | EqB  Logic Logic
          | NeqB Logic Logic
          | Math Math
          
data Math = InMath Math
          | Exp Math Math 
          | Mult Math Math 
          | Plus Math Math
          | Minus Math Math 
          | Number Builder
          | IfMath Logic Math Math
          | LEQ' Math Math | LT' Math Math 
          | GEQ' Math Math | GT' Math Math 
          | EQ' Math Math  | NEQ' Math Math

{- Function to convert from 'Logic' to 'Builder' 
    Input:    A Logic value arranged in a parse tree format 
              such that it's precedence follows boolean laws
    Process:  The function goes down the parse trees and
              uses the 'ap' function to connect the nodes
              on the parse tree
-}
parseLogic :: Logic -> Builder
parseLogic (InLgc a)  =                 parseLogic a
parseLogic (Not a)    = ap notBdr      (parseLogic a)
parseLogic (And a b)  = apB andBdr     (parseLogic a) (parseLogic b)
parseLogic (Or a b)   = apB orBdr      (parseLogic a) (parseLogic b)
parseLogic (If a b c) = (apB . ap ifBdr) (parseLogic a) (parseLogic b) (parseLogic c)
parseLogic (Bool a)   = a
parseLogic (EqB a b)  = apB eqBoolBdr  (parseLogic a) (parseLogic b)
parseLogic (NeqB a b) = apB neqBoolBdr (parseLogic a) (parseLogic b)
parseLogic (Math a)   = parseMath a

parseMath :: Math -> Builder
parseMath (InMath a)     =               parseMath a
parseMath (Exp a b)      = apB expBdr   (parseMath a) (parseMath b)
parseMath (Mult a b)     = apB multBdr  (parseMath a) (parseMath b)
parseMath (Plus a b)     = apB addBdr   (parseMath a) (parseMath b)
parseMath (Minus a b)    = apB minusBdr (parseMath a) (parseMath b)
parseMath (Number a)     = a
parseMath (IfMath l a b) = (apB . ap ifBdr) (parseLogic l) (parseMath a) (parseMath b)
parseMath (LEQ' a b)     = apB leqBdr (parseMath a) (parseMath b)
parseMath (LT' a b)      = apB ltBdr  (parseMath a) (parseMath b)
parseMath (GEQ' a b)     = apB geqBdr (parseMath a) (parseMath b)
parseMath (GT' a b)      = apB gtBdr  (parseMath a) (parseMath b)
parseMath (EQ' a b)      = apB eqBdr  (parseMath a) (parseMath b)
parseMath (NEQ' a b)     = apB neqBdr (parseMath a) (parseMath b)

digit :: Parser Char
digit = satisfy isDigit

-- \| Exercise 1
-- ===============================
-- Builder Expressions for True, False, If, and, or and not
-- ===============================
trueBdr, falseBdr :: Builder
trueBdr  = boolToLam True
falseBdr = boolToLam False

ifBdr, andBdr, orBdr, notBdr :: Builder
ifBdr  = lamify "btf" $ termify "btf"
andBdr = lamify "xy"  $ ifBdr `ap` term 'x' `ap` term 'y' `ap` falseBdr -- λxy. IF x  y FALSE
orBdr  = lamify "xy"  $ ifBdr `ap` term 'x' `ap` trueBdr `ap` term 'y' -- λxy. IF x TRUE y
notBdr = 'x' `lam`      ifBdr `ap` term 'x' `ap` falseBdr `ap` trueBdr -- λx. IF x FALSE TRUE

-- Applying the 'and' or 'or' Builders
applyAnd, applyOr :: Chainer
applyAnd = apB andBdr
applyOr  = apB orBdr

-- \| Exercise 2
-- ===============================
-- Builder Expressions for zero, succ and pred
-- ===============================
getNum :: String -> Builder
getNum s = intToLam (read s :: Int)

zeroBdr :: Builder
zeroBdr = lamify "fx" $ term 'x'

succBdr, predBdr :: Builder
succBdr = lamify "nfx" $ term 'f' `ap` termify "nfx"
predBdr = lamify "nfx" $ term 'n' `ap` predPart1 `ap` predPart2 `ap` predPart3
  where
    predPart1 = lamify "gh" $ term 'h' `ap` termify "gf"
    predPart2 = 'u' `lam` term 'x'
    predPart3 = 'u' `lam` term 'u'

-- ===============================
-- Builder Expressions for multipler and exponent
-- ===============================
addBdr, minusBdr, multBdr, expBdr :: Builder
addBdr   = lamify "xy"  $ term 'y' `ap` succBdr `ap` term 'x'
minusBdr = lamify "xy"  $ term 'y' `ap` predBdr `ap` term 'x'
multBdr  = lamify "xyf" $ term 'x' `ap` termify "yf" -- x * y = multiply = λxyf.x(yf)
expBdr   = lamify "xy"  $ termify "yx" -- x ** y = exp = λxy.yx

-- \| Exercise 3
-- ===============================
-- Builder Expressions for GT>, LT<, LEQ<=, GEQ>=, EQ==, NEQ!=
-- ===============================
eqParams :: Params
eqParams = lamify "mn"

m, n, leqmn :: Builder
m = term 'm'
n = term 'n'
leqmn = apB leqBdr m n

leqBdr, ltBdr, geqBdr, gtBdr, eqBdr, neqBdr :: Builder
leqBdr = eqParams $ isZeroBdr `ap` apB minusBdr m n                  -- λmn.isZero (minus m n)
ltBdr  = eqParams $ notBdr    `ap` apB geqBdr m n                    -- λmn.not    (GEQ m n)
geqBdr = eqParams $ isZeroBdr `ap` apB minusBdr n m                  -- λmn.isZero (minus n m)
gtBdr  = eqParams $ notBdr    `ap` leqmn                             -- λmn.not    (LEQ m n)
eqBdr  = eqParams $ andBdr    `ap` leqmn `ap` (leqBdr `ap` n `ap` m) -- λmn.and    (LEQ m n) (LEQ n m)
neqBdr = eqParams $ notBdr    `ap` apB eqBdr m n                     -- λmn.not    (EQ m n)

-- ===============================
-- Builder Expressions for EQ and NEQ (for boolean values)
-- ===============================
-- λmn.IF m (IF n True False) (IF (not n) True False)
eqBoolBdr :: Builder
eqBoolBdr = eqParams $ ifBdr `ap` m `ap` trueEq `ap` falseEq
    where
        trueEq = ifBdr `ap` n `ap` trueBdr `ap` falseBdr
        falseEq = ifBdr `ap` n `ap` falseBdr `ap` trueBdr
        
neqBoolBdr :: Builder
neqBoolBdr = eqParams $ notBdr `ap` apB eqBoolBdr m n

-- ===============================
-- Builder Expression for isZero
-- ===============================
-- λn.n(λx.False)True
isZeroBdr :: Builder
isZeroBdr = 'n' `lam` term 'n' `ap` ('x' `lam` falseBdr) `ap` trueBdr

{- Part 3 -}
-- \| Exercise 1
-- ===============================
-- Builder Expressions
-- ===============================
data ConsList = List Elem ConsList | Nil
data Elem = Elem Logic | EList ConsList

parseList :: ConsList -> Builder
parseList (List first rest) = apB consBdr (parseElem first) (parseList rest)
parseList Nil               = nullBdr

parseElem :: Elem -> Builder
parseElem (Elem x)  = parseLogic x
parseElem (EList x) = parseList x

insideList :: Parser a -> Parser a
insideList = between (isTok '[') (isTok ']')

nullBdr, consBdr, isNullBdr :: Builder
nullBdr   = lamify "cn"   $ term 'n'
consBdr   = lamify "htcn" $ termify "ch" `ap` termify "tcn"
isNullBdr = 'l' `lam`       term 'l' `ap` lamify "ht" falseBdr `ap` trueBdr

headBdr, tailBdr :: Builder
headBdr = 'l' `lam`      term 'l' `ap` lamify "ht" (term 'h') `ap` falseBdr
tailBdr = lamify "lcn" $ term 'l' `ap` tailPart1 `ap` tailPart2 `ap` tailPart3
  where
    tailPart1 = lamify "ht" $ 'g' `lam` termify "gh" `ap` termify "tc"
    tailPart2 = 't' `lam` term 'n'
    tailPart3 = lamify "ht" $ term 't'

-- \| Exercise 2
-- ===============================
-- Z-Combinators
-- ===============================
zMaker :: Builder -> Builder
zMaker b = lam 'm' $ ap b b

z, z2, z3 :: Builder
z  = zMaker $ 'x' `lam` term 'm' `ap` ('v' `lam` termify "xxv")
z2 = zMaker $ 'x' `lam` term 'm' `ap` lamify "fl" (termify "xxfl")
z3 = zMaker $ 'x' `lam` term 'm' `ap` lamify "fil" (termify "xxfil")

-- ===============================
-- Factorial Function as a Lambda Expression
-- ===============================
oneBdr, facBdr :: Builder
oneBdr = succBdr `ap` zeroBdr
facBdr = lamify "fn" $ ifBdr `ap` cond `ap` oneBdr `ap` elseFac
  where
    cond = isZeroBdr `ap` term 'n'
    elseFac = multBdr `ap` term 'n' `ap` recCall
    recCall = term 'f' `ap` (predBdr `ap` term 'n')

-- ===============================
-- Map Function as a Lambda Expression
-- ===============================
mapBdr :: Builder
mapBdr = lamify "mfl" $ ifBdr `ap` cond `ap` nullBdr `ap` elseMap
  where
    cond = isNullBdr `ap` term 'l'
    elseMap = apB consBdr headMap restMap
    headMap = term 'f' `ap` (headBdr `ap` term 'l')
    restMap = termify "mf" `ap` (tailBdr `ap` term 'l')

-- ===============================
-- Foldr Function as a Lambda Expression
-- ===============================
foldrBdr :: Builder
foldrBdr = lamify "gfil" $ ifBdr `ap` cond `ap` term 'i' `ap` elseFold
  where
    cond = isNullBdr `ap` term 'l'
    elseFold = apB (term 'f') headFold restFold
    headFold = headBdr `ap` term 'l'
    restFold = termify "gfi" `ap` (tailBdr `ap` term 'l')

-- ===============================
-- Filter Function as a Lambda Expression (not used but looks cool so I'm leaving it here)
-- ===============================
filterBdr :: Builder
filterBdr = lamify "gfl" $ ifBdr `ap` cond `ap` trueFilt `ap` elseFilt
  where
    cond = term 'f' `ap` headFilt
    trueFilt = consBdr `ap` headFilt `ap` elseFilt
    elseFilt = term 'g' `ap` term 'f' `ap` rest
    rest = tailBdr `ap` term 'l'
    headFilt = headBdr `ap` term 'l'