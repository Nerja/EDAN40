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
vars (Var name)   = [name]
vars (And e1 e2)  = vars e1 `Data.List.union` vars e2
vars (Or e1 e2)   = vars e1 `Data.List.union` vars e2
vars (Not e)      = vars e

truthValue :: Expr -> [(String, Bool)] -> Bool
truthValue (Var n) table      = fromMaybe False $ lookup n table
truthValue (And p1 p2) table  = truthValue p1 table && truthValue p2 table
truthValue (Or p1 p2) table   = truthValue p1 table || truthValue p2 table
truthValue (Not p) table      = not $ truthValue p table

-- 2.1.3
tautology :: Expr -> Bool
tautology p = all (truthValue p) $ allComb $ vars p
  where allComb :: [String] -> [[(String, Bool)]]
        allComb []      = [[]]
        allComb (v:vs)  = [(v, val):rec | rec <- allComb vs, val <- [True, False]]

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
addElem x (Set xs) = Set $ [x] `Data.List.union` xs

containsElem :: (Eq a) => Set a -> a -> Bool
containsElem (Set xs) = flip elem xs

union :: (Eq a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set $ xs `Data.List.union` ys

removeElem :: (Eq a) => a -> Set a -> Set a
removeElem x (Set xs) = Set $ xs \\ [x]

-- Fast set
newtype SortSet a = SortSet [a]
emptySort :: SortSet a
emptySort = SortSet []

addElemSort :: (Ord a) => a -> SortSet a -> SortSet a
addElemSort x set@(SortSet xs)
  | set `containsElemSort` x  = set
  | otherwise                 = SortSet $ x `insert` xs

unionSort :: (Ord a) => SortSet a -> SortSet a -> SortSet a
unionSort (SortSet xs) (SortSet ys) = SortSet $ xs `fastUnion` ys
  where fastUnion :: (Ord a) => [a] -> [a] -> [a]
        fastUnion xs []         = xs
        fastUnion [] ys         = ys
        fastUnion (x:xs) (y:ys)
          | x == y    = fastUnion xs (y:ys)
          | x < y     = x : fastUnion xs (y:ys)
          | otherwise = y : fastUnion (x:xs) ys

containsElemSort :: (Ord a) => SortSet a -> a -> Bool
containsElemSort (SortSet xs) x = fastContains x xs 0 (length xs)
  where fastContains :: (Ord a) => a -> [a] -> Int -> Int -> Bool
        fastContains x xs s e
          | s >= e        = False
          | x == xs !! m  = True
          | otherwise     = fastContains x xs s (m-1) || fastContains x xs (m+1) e
          where m = (s+e) `div` 2

-- 2.4
--instance (Ord a, Ord b) => Ord (a,b) where
--  compare (a,b) (c,d)
--    | a < c       = LT
--    | a > c       = GT
--    | b < d       = LT
--    | b > d       = GT
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
