import Data.Char
import Prelude hiding (fail, return, iterate)
type Parser a = String -> Maybe (a, String)

-- Ex 1
semicolon' :: Parser Char
semicolon' (';':xs) = Just (';', xs)
semicolon' _        = Nothing

-- Ex 2
becomes' :: Parser String
becomes' (':':'=':xs) = Just (":=", xs)
becomes' _            = Nothing

-- Ex 3
char :: Parser Char
char [] = Nothing
char (x:xs) = Just (x,xs)

-- Code from instructions

fail :: Parser a
fail _ = Nothing

return :: a -> Parser a
return a cs = Just (a,cs)

infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs = case m cs of
              Nothing -> Nothing
              Just (a, cs) -> if (p a) then Just (a, cs) else Nothing

digit :: Parser Char
digit = char ? isDigit

infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
             Nothing -> n cs
             mcs -> mcs

-- Ex 4
letter :: Parser Char
letter = char ? isAlpha

space :: Parser Char
space = char ? isSpace

-- Ex 5
alphanum :: Parser Char
alphanum = digit ! letter

-- Code from instructions
lit :: Char -> Parser Char
lit = (char ?) . (==)

-- Ex 6
semicolon :: Parser Char
semicolon = lit ';'

-- Code from instructions
{-
infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs = case m cs of
              Nothing -> Nothing
              Just (r, cs') -> case n cs' of
                               Nothing -> Nothing
                               Just (r', cs'') -> Just ((r,r'), cs'')
-}

twochars :: Parser (Char, Char)
twochars = char # char

-- Ex 7
becomes :: Parser (Char, Char)
becomes = (lit ':') # (lit '=')

becomes'' :: Parser (Char, Char)
becomes'' = flip (?) (==(':','=')) $ twochars

-- Code from instructions
infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> k) cs = case m cs of
                  Nothing -> Nothing
                  Just (a, cs') -> Just (k a, cs')

digitVal :: Parser Int
digitVal = digit >-> digitToInt

-- Ex 8
upperLetter :: Parser Char
upperLetter = letter >-> toUpper

-- Ex 9
sndchar :: Parser Char
sndchar = twochars >-> snd

-- Ex 10
twochars' :: Parser String
twochars' = twochars >-> (\(a,b) -> a:b:[])

-- Ex 11
infixl 6 -#
(-#) :: Parser a -> Parser b -> Parser b
(-#) m n = m # n >-> snd

-- Ex 12
infixl 6 #-
(#-) :: Parser a -> Parser b -> Parser a
(#-) m n = m # n >-> fst

-- Code from instructions
iterate :: Parser a -> Int -> Parser [a]
iterate m 0 = return []
iterate m i = m # iterate m (i-1) >-> (\(hd, tl) -> hd:tl)

iter :: Parser a -> Parser [a]
iter m = ((m # iter m) >-> cons) ! (return [])

cons = (\(hd, tl) -> hd:tl)

-- Ex 13
-- return misslyckas aldrig!

-- Code from instructions
letters :: Parser String
letters = letter # iter letter >-> cons

token :: Parser a -> Parser a
token m = m #- iter space

word :: Parser String
word = token letters

accept :: String -> Parser String
accept w = token (iterate char (length w) ? (==w))

-- Code from instructions
infix 4 #>
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(m #> k) cs = case m cs of
                Nothing -> Nothing
                Just (a, cs') -> k a cs'

double :: Parser Char
double = char #> lit

bldNumber :: Int -> Int -> Int
bldNumber n d = 1*n + d

number' :: Int -> Parser Int
number' n = digitVal >-> bldNumber n #> number' ! return n

number :: Parser Int
number = token (digitVal #> number')

-- Ex 14
infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(#) m n = m #> (\a -> n >-> (\x -> (a,x)))

-- Code from instructions
data Expr =
  Num Int | Var String | Add Expr Expr |
  Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = lit '*' >-> (\_ -> Mul) ! lit '/' >-> (\_ -> Div)

addOp :: Parser (Expr -> Expr -> Expr)
addOp = lit '+' >-> (\_ -> Add) ! lit '-' >-> (\_ -> Sub)

var :: Parser Expr
var = word >-> Var

num :: Parser Expr
num = number >-> Num

factor :: Parser Expr
factor = num ! var ! lit '(' -# var #- lit ')'

term' :: Expr -> Parser Expr
term' e = mulOp # factor >-> bldOp e #> term' ! return e

bldOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
bldOp e (oper, e') = oper e e'
