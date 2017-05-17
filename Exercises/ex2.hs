import Data.Maybe
import Control.Monad
import Data.List
-- EDAN40
-- 2.1.1
data Expr = Var String
          | And Expr Expr
          | Or Expr Expr
          | Not Expr

-- 2.1.2
vars :: Expr -> [String]
vars (Var name) = [name]
vars (And e1 e2) = vars e1 ++ vars e2
vars (Or e1 e2) = vars e1 ++ vars e2
vars (Not e) = vars e

varLookup :: String -> [(String, Bool)] -> Bool
varLookup var = (fromMaybe False) . lookup var

truthValue :: Expr -> [(String, Bool)] -> Bool
truthValue (Var v) table = varLookup v table
truthValue (And e1 e2) table = truthValue e1 table && truthValue e2 table
truthValue (Or e1 e2) table = truthValue e1 table || truthValue e2 table
truthValue (Not e) table = not $ truthValue e table

-- 2.1.3
tautology :: Expr -> Bool
tautology expr = and $ map (truthValue expr) $ allPos $ vars expr

allPos :: [String] -> [[(String, Bool)]]
allPos [] = [[]]
allPos (v:vs) = [(v, value) : rec | rec <- allPos vs, value <- [True, False]]

-- 2.2.1
data Item = File String
          | Directory String [Item]
          deriving (Show, Eq)

-- 2.2.2
lookupItem :: String -> Item -> Maybe String
lookupItem str (File n) = if str == n then (Just n) else Nothing
lookupItem str (Directory n items)
    | str == n      = Just (n ++ "/")
    | otherwise     = fmap ((n++).('/':)) $ msum $ map (lookupItem str) items

a = File "a"
c = File "c"
x = File "x"
y = File "y"
b = Directory "b" [x,y]
tilde = Directory "~" [a,b,c]

-- 2.3.1
data Set a = Set [a]

instance (Show a) => Show (Set a) where
  show (Set list) = foldr ((++).(++",").show) "" list

emptySet :: (Eq a) => Set a
emptySet = Set []

addElem :: (Eq a) => a -> Set a -> Set a
addElem el (Set list)
    | elem el list            = Set list
    | otherwise               = Set (el:list)

hasElem :: (Eq a) => a -> Set a -> Bool
hasElem el (Set list) = elem el list

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set l1) (Set l2) = Set $ union l1 l2

removeElem :: (Eq a) => a -> Set a -> Set a
removeElem el (Set list) = Set $ filter(/=el) list

-- 2.4
--instance (Ord a, Ord b) => Ord (a,b) where
--  compare (a,b) (c,d)
--    | a < b       = LT
--    | a > b       = GT
--    | b < c       = LT
--    | b > c       = GT
--    | otherwise   = EQ

--instance (Ord b) => Ord [b] where
--  compare [] _  = LT
--  compare _ []  = GT
--  compare [] [] = EQ
--  compare (x:xs) (y:ys)
--    | x == y    = compare xs ys
--    | x < y     = LT
--    | otherwise = GT

-- 2.5
newtype ListNatural = ListNatural [()]

oneL = [()]
twoL = [(),()]
threeL = [(),(),()]

-- (:) is plus one by () : oneL ~> [(),()]
-- (++) is addition
-- map (const ()) is id

-- f1 is +
-- f2 is x * y
-- f3 is ^

-- 2.5.2
instance Num ListNatural where
  (+) (ListNatural xs) (ListNatural ys)     = ListNatural $ xs ++ ys
  (*) (ListNatural xs) (ListNatural ys)     = ListNatural $ foldr (const(++xs)) [] ys
  negate                                    = error "Nahhh"
  abs                                       = id
  signum (ListNatural xs)                   = if (length xs > 0) then 1 else 0
  fromInteger                               = ListNatural . map (const ()) . enumFromTo 1
