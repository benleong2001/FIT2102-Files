module LambdaParser where

import Control.Applicative
import Data.Builder
import Data.Lambda
import HelperFile
import Parser
import Prelude

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.

-- |
--    Part 1

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('
--
-- >>> parse longLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse longLambdaP "xx"
-- UnexpectedChar 'x'
--
-- >>> parse longLambdaP "(λb.(λt.(λf.ftb)))"
-- Result >< \btf.ftb
--
-- >>> parse longLambdaP "(λx.x(xx)x)"
-- Result >< \x.x(xx)x
--
-- >>> parse longLambdaP "(λx.(λy.x))"
-- Result >< \xy.x
--
-- >>> parse longLambdaP "(λx.(λy.x(x(xy))))"
-- Result >< \xy.x(x(xy))
--
-- >>> parse longLambdaP "(λx.x(xx))"
-- Result >< \x.x(xx)
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)y))"
-- Result >< \xy.xy(xx)y
--
-- >>> parse longLambdaP "((λx.x(xx)x)(λy.y(yy)y))"
-- Result >< (\x.x(xx)x)\y.y(yy)y
--
-- >>> parse longLambdaP "(λx.x(xx)x)(λy.y(yy)y)"
-- Result >< (\x.x(xx)x)\y.y(yy)y
--
-- >>> parse longLambdaP "((λx.xx)(λy.yy))"
-- Result >< (\x.xx)\y.yy
--
-- >>> parse longLambdaP "((λx.x)(λx.x))"
-- Result >< (\x.x)\x.x
--
-- >>> parse longLambdaP "((λz.z)(λz.z)(λz.z))"
-- Result >< (\z.z)(\z.z)\z.z
--
-- >>> parse longLambdaP "((λx.x)(λy.(yy)y)(λz.z(zz)))"
-- Result >< (\x.x)(\y.yyy)\z.z(zz)
--
-- >>> parse longLambdaP "((λx.x)(λy.yyy)(λz.z(zz)))"
-- Result >< (\x.x)(\y.yyy)\z.z(zz)
--
-- >>> parse longLambdaP "(λbt.(λf.btf)))"
-- UnexpectedChar 't'
--
-- >>> parse longLambdaP "((λz.z)(λz.z)(λz.z)(λz.z))"
-- Result >< (\z.z)(\z.z)(\z.z)\z.z
--
-- >>> parse longLambdaP "(λx.(λy.x(λz.z)))"
-- Result >< \xy.x\z.z

-- ===============================
-- These parsers here follow the LongLambda section of the BNF
-- ===============================
longBase :: Parser Builder
longBase = betwBrac $ param <*> chain longBody (pure ap)

longBody :: Parser Builder
longBody = betwBrac longBody ||| chain inner (pure ap)
  where
    inner =
      (term <$> alpha)
        ||| longLambda
        ||| betwBrac longBody

longLambda :: Parser Builder
longLambda = spaces *> (betwBrac longLambda ||| chain longBase (pure ap))

longLambdaP :: Parser Lambda
longLambdaP = build <$> longLambda

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "λx.xλy.yy"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> isErrorResult (parse shortLambdaP "λxyz")
-- True
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)(λz.zzz)"
-- Result >< (\x.x)(\y.yy)\z.zzz
--
-- >>> parse shortLambdaP "(λx.x(xx))"
-- Result >< \x.x(xx)
--
-- >>> parse shortLambdaP "λx.x(xx)x"
-- Result >< \x.x(xx)x
--
-- >>> parse shortLambdaP "λx.x(λy.yy).y"
-- Result >.y< \x.x\y.yy
--
-- >>> parse shortLambdaP "λx.λy.y"
-- Result >< \xy.y
--
-- >>> parse shortLambdaP "λx.(λy.y)"
-- Result >< \xy.y
--
-- >>> parse shortLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(xx)x(λy.y(yy)y)"
-- Result >< \x.x(xx)x\y.y(yy)y
--
-- >>> parse shortLambdaP "(λx.x)λy.yy"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.y)(λx.xx)"
-- Result >< (\x.x)(\y.y)\x.xx
--
-- >>> parse shortLambdaP "λx.xλy.yλz.zz"
-- Result >< \x.x\y.y\z.zz
--
-- >>> parse shortLambdaP "λxyz.xx(y) (λf.λg.fgg)"
-- Result >< \xyz.xxy\fg.fgg
--
-- >>> parse shortLambdaP "(λx.(λy.xy λabc.c) (λstu.u(ts)))"
-- Result >< \x.(\y.xy\abc.c)\stu.u(ts)
--
-- >>> parse shortLambdaP "(λxy.(λz.xyz))"
-- Result >< \xyz.xyz
--
-- >>> parse shortLambdaP "λx.λy.λz.xyz"
-- Result >< \xyz.xyz

-- ===============================
-- These parsers here follow the ShortLambda section of the BNF
-- ===============================
shortBase :: Parser Builder
shortBase = betwBrac shortBase ||| (params <*> chain shortBody (pure ap))

shortBody :: Parser Builder
shortBody = betwBrac shortBody ||| chain inner (pure ap)
  where
    inner =
      betwBrac shortLambda
        ||| shortLambda
        ||| (term <$> alpha)
        ||| betwBrac shortBody

shortLambda :: Parser Builder
shortLambda = spaces *> (chain shortBase (pure ap) ||| betwBrac shortLambda)

shortLambdaP :: Parser Lambda
shortLambdaP = build <$> (shortLambda ||| betwBrac shortLambda)

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> isErrorResult (parse lambdaP "λx..x")
-- True

-- ===============================
-- This parser tries to parse long form expressions first.
-- If it fails, it then tries short form.
-- If both fails, the input is not a lambda expression.
-- ===============================
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

-- |
--    Part 2
-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

-- ===============================
-- Parsers for the Logical Connectives: And, Or, Not, If
-- ===============================
andP, orP :: Parser ChainL
andP = strParse "and" $ pure And
orP  = strParse "or"  $ pure Or

notP, ifExprP :: Parser Logic
notP    = strParse "not" $ Not <$> simpleP
ifExprP = liftA3 If b t f
  where
    b = strParse "if"   logicExpr
    t = strParse "then" logicExpr
    f = strParse "else" logicExpr

-- ===============================
-- Parser for True and False
-- ===============================
trueP, falseP, boolP :: Parser Logic
trueP  = strParse "True"  $ pure $ Bool trueBdr
falseP = strParse "False" $ pure $ Bool falseBdr
boolP  = trueP ||| falseP

-- ===============================
-- Parser for Logical Expressions
-- ===============================
bracP, simpleP, andExprP, orExprP :: Parser Logic
bracP    = betwBrac $ InLgc <$> simpleP
simpleP  = bracP
        ||| notP 
        ||| boolP
andExprP = chain simpleP andP
orExprP  = chain andExprP orP

-- ===============================
-- Final Parser to put all the pieces together
-- ===============================
logicExpr :: Parser Logic
logicExpr = spaces *> (orExprP ||| ifExprP)

logicP :: Parser Lambda
logicP = build . parseLogic <$> logicExpr

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

-- ===============================
-- Parsers for Operators
-- ===============================
-- Modularise function to return the constructor corresponding to the input string
op :: String -> Parser ChainM
op s = strParse s $ pure oper
  where
    oper
      | s == "+"  = Plus
      | s == "-"  = Minus
      | s == "*"  = Mult
      | otherwise = Exp

plusMinusP :: Parser ChainM
plusMinusP = op "+" ||| op "-"

-- ===============================
-- Parsers for Basic Math expressions
-- With integers, + or - only
-- ===============================
bracMath, operP, number :: Parser Math

-- Returns a math expression as an InMath value
bracMath = betwBrac $ InMath <$> operP

-- Reads an integer and returns it as a Number value
number   = Number . getNum <$> (list1 digit <* spaces)  

-- Chains number with either + or - and returns the result
operP    = bracMath ||| chain number plusMinusP

-- ===============================
-- Finally, the main parser
-- ===============================
basicArithmeticP :: Parser Lambda
basicArithmeticP = build . parseMath <$> operP

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

-- ===============================
-- Parsers for multiplication and exponential
-- ===============================
{- Returns the Mult and Exp constructors -}
mult, expo :: Parser ChainM
mult = op "*"
expo  = op "**"

-- ===============================
-- Parsers for expressions with **, * and (+ or -) operations
-- ===============================
bracArith, expP, multExprP, arithExprP  :: Parser Math
bracArith  = betwBrac $ InMath <$> arithExprP
expP       = chain (bracArith ||| number) expo
multExprP  = chain expP                   mult
arithExprP = chain multExprP              plusMinusP

arithmeticP :: Parser Lambda
arithmeticP = build . parseMath <$> arithExprP

-- | Exercise 3
-- |
-- | The church encoding for comparison operations are given below (with x and y being church numerals)
-- |
-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)
-- |
-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True
-- |
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "9 * 2 > 3 * 6"
-- Result >< Just False
--
-- >>> lamToBool <$> parse complexCalcP "4 * 2 < 3 ** 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "6 + 9 == 3 * 5"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "20 - 18 >= 16 + 30 - 20"
-- Result >< Just False

-- ===============================
-- Chaining Parsers (Inequalities with integers)
-- ===============================
leqP, ltP, geqP, gtP, eqP, neqP, allEqPs :: Parser ChainM
leqP    = strParse "<=" $ pure LEQ'
ltP     = strParse "<"  $ pure LT'
geqP    = strParse ">=" $ pure GEQ'
gtP     = strParse ">"  $ pure GT'
eqP     = strParse "==" $ pure EQ'
neqP    = strParse "!=" $ pure NEQ'
allEqPs = leqP ||| ltP ||| geqP ||| gtP ||| eqP ||| neqP

-- ===============================
-- Chaining Parsers (Inequalities with booleans)
-- ===============================
eqBoolP, neqBoolP :: Parser ChainL
eqBoolP  = strParse "==" $ pure EqB
neqBoolP = strParse "!=" $ pure NeqB

-- ===============================
-- Arithmatic Operations
-- ===============================
bracComp, expComp, multComp, arithComp, ifComp, mathIneq  :: Parser Math
bracComp  = betwBrac $ InMath <$> arithComp
expComp   = chain (bracComp ||| number) expo
multComp  = chain expComp mult
arithComp = ifComp ||| chain multComp plusMinusP
ifComp    = liftA3 IfMath b t f
  where
    b = strParse "if"   logicExpr
    t = strParse "then" arithComp
    f = strParse "else" arithComp
mathIneq = chain arithComp allEqPs

-- ===============================
-- Inequalities and Boolean Operations
-- ===============================
boolIneq :: Parser Logic
boolIneq = chain logicExpr $ eqBoolP ||| neqBoolP

ineqs :: Parser Logic
ineqs = (Math <$> mathIneq) ||| boolIneq

-- ===============================
-- Connecting the inequalities together
-- ===============================
andConnectP, orConnectP :: Parser Logic
andConnectP = chain (ineqs ||| ifConnectP) andP
orConnectP  = chain andConnectP orP

ifConnectP :: Parser Logic
ifConnectP = liftA3 If b t f
  where
    b = strParse "if"   orConnectP
    t = strParse "then" orConnectP
    f = strParse "else" orConnectP

-- ===============================
-- Final complex Parsers
-- ===============================
complexCalcB :: Parser Logic
complexCalcB = ifConnectP ||| orConnectP

complexCalcP :: Parser Lambda
complexCalcP = build . parseLogic <$> complexCalcB

-- |
--    Part 3
-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof

-- ===============================
-- Parsers to identify and form cons lists
-- ===============================
elemL, elemE, elemP :: Parser Elem
elemL = Elem <$> complexCalcB
elemE = EList <$> listB
elemP = elemL ||| elemE

elems1P, elemsP :: Parser [Elem]
elems1P = liftA2 (:) elemP $ list (isTok ',' >> elemP)
elemsP  = elems1P ||| pure []

listElems :: Parser ConsList
listElems = foldr ($) Nil <$> ((List <$>) <$> elemsP)

-- ===============================
-- Final Parser lists
-- ===============================
listB :: Parser ConsList
listB = insideList listElems

listP :: Parser Lambda
listP = build . parseList <$> listB

-- |
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False

-- ===============================
-- Parsers for list operations
-- ===============================
headP, tailP, isNullP :: Parser Builder
headP   = strParse "head"   $ ap headBdr <$> listOpB
tailP   = strParse "rest"   $ ap tailBdr <$> listOpB
isNullP = strParse "isNull" $ ap isNullBdr <$> listOpB

-- ===============================
-- Final Parsers for list operators
-- ===============================
listOpB :: Parser Builder
listOpB = headP ||| tailP ||| isNullP ||| (parseList <$> listB)

listOpP :: Parser Lambda
listOpP = build <$> listOpB

-- | Exercise 2

-- | Implement your function(s) of choice below!

-- ===============================
-- Parser for factorial function
-- ===============================
fac :: Parser Builder
fac = spaces *> (ap (z `ap` facBdr) <$> (parseMath <$> (bracArith ||| number))) <* isTok '!'

facP :: Parser Lambda
facP = build <$> fac

-- ===============================
-- Parser for map function
-- ===============================
mapB :: Parser Builder
mapB = spaces >> stringTok "map" *> liftA2 (apB (z2 `ap` mapBdr)) arithFunc listOpB

mapP :: Parser Lambda
mapP = build <$> mapB

-- ===============================
-- Functions to input into the map function
-- ===============================
addMap, minusMap, multMap  :: Parser Builder
addMap   = isTok '+' >> pure addBdr
minusMap = isTok '-' >> pure minusBdr
multMap  = isTok '*' >> pure multBdr

leftArith, rightArith :: Parser Builder
leftArith  = liftA2 (flip ap) (parseMath <$> number) (addMap ||| minusMap ||| multMap)
rightArith = liftA2 ap        (addMap ||| minusMap ||| multMap) (parseMath <$> number)

arithFunc :: Parser Builder
arithFunc = betwBrac (leftArith ||| rightArith)

-- ===============================
-- Parser for foldr function
-- ===============================
{- 
  This implementation of foldr can only take +, -, *, and, or as its functions 
-}
foldrGeneral :: Parser Builder -> Parser Builder -> Parser Builder
foldrGeneral p1 p2 = spaces >> stringTok "foldr" *> liftA3 (apB . ap (z3 `ap` foldrBdr)) p1 p2 listOpB

foldrArith :: Parser Builder
foldrArith = foldrGeneral foldrArithFunc foldrArithInit 

foldrLogic :: Parser Builder
foldrLogic = foldrGeneral foldrLogicFunc foldrLogicInit

foldrB :: Parser Builder
foldrB = foldrArith ||| foldrLogic

foldrP :: Parser Lambda
foldrP = build <$> foldrB

foldrArithFunc :: Parser Builder
foldrArithFunc = betwBrac $ addMap ||| minusMap ||| multMap

foldrArithInit :: Parser Builder
foldrArithInit = parseMath <$> arithExprP

foldrLogicFunc :: Parser Builder
foldrLogicFunc = betwBrac $ (stringTok "and" >> pure andBdr) ||| (stringTok "or" >> pure orBdr)

foldrLogicInit :: Parser Builder
foldrLogicInit = parseLogic <$> logicExpr
