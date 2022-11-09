module Calculator
  ( parseCalc
  ) where
import           Instances
import           Parser

data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Number Int
 deriving Show

op :: Char -> Parser Char -- parse a single char operator
op c = spaces >> charTok c >> pure c

number :: Parser Expr     -- parse a Number
number = spaces >> Number <$> int

add :: Parser (Expr -> Expr -> Expr)
add = op '+' >> pure Plus

minus :: Parser (Expr -> Expr -> Expr)
minus = op '-' >> pure Minus

times :: Parser (Expr -> Expr -> Expr)
times = op '*' >> pure Times

-- | Term that should be evaluated before expressions
term :: Parser Expr
term = chain number times

-- | Expression consisting of terms and lower precedence operators
expr :: Parser Expr
expr = chain term (add ||| minus)

-- | Parse an expression of integers, +, -, *
-- >>> parseCalc " 12 + 21* 3 "
-- Result > < Plus (Number 12) (Times (Number 21) (Number 3))
--
-- >>> parseCalc " 6 *4 + 333- 8 *  24"
-- Result >< Minus (Plus (Times (Number 6) (Number 4)) (Number 333)) (Times (Number 8) (Number 24))
parseCalc :: String -> ParseResult Expr
parseCalc = parse expr
