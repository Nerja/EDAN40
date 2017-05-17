-- Exam 2015-01-13
import Data.Maybe

-- Assignment 1
f = (/) . (+3)
g = flip map . flip take (iterate (+1) 1)

-- Assignment 2
{-
(8-)      :: Num a => a -> a
(+0).(0+) :: Num a => a -> a
(.)(.)    :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
(($)$($)) :: (a -> b) -> a -> b
([]>>=)(\_ -> [(>=)])
-}

-- Assignment 3
{-
  spark: Potential to create a thread
  seq:   Hint that the first argument should be evaluated. The second argument
         is returned.
  pseq:  Forces the first argument to be evaluated. The second argument is returned.
  par:   Evaulation of the first argument may be done in parallel with returning the
         second argument.
-}

-- Assignment 4
newtype CircList = CircList [Int]

perimeter :: CircList -> Int
perimeter (CircList xs) = length xs

currentElem :: CircList -> Maybe Int
currentElem (CircList xs) = listToMaybe xs

nextElem :: CircList -> Maybe Int
nextElem (CircList xs)
  | null xs         = Nothing
  | length xs == 1  = Just $ head xs
  | otherwise       = listToMaybe $ tail xs

prevElem :: CircList -> Maybe Int
prevElem (CircList xs)
  | null xs         = Nothing
  | length xs == 1  = Just $ head xs
  | otherwise       = listToMaybe $ reverse xs

insert :: CircList -> Int -> CircList
insert (CircList xs) x = CircList $ xs ++ [x]

takeFromCL :: Int -> CircList -> [Int]
takeFromCL n cl@(CircList xs)
  | n <= 0    = []
  | otherwise = take n xs ++ takeFromCL (n - length xs) cl

equalCircList (CircList xs) (CircList ys) = any (==ys) $ revs ++ fors
  where revs = take (length xs) $ iterate sRev xs
        fors = take (length xs) $ iterate sFor xs

sRev, sFor :: [a] -> [a]
sRev []     = []
sRev [x]    = [x]
sRev xs@(x:_) = last xs : init xs

sFor []     = []
sFor [x]    = [x]
sFor (x:xs) = xs ++ [last xs]

-- Assignment 5
{-
  Type is:
  g :: Monad m => m a -> m b -> m (a, b)

  Values is:
  1)  Nothing
  2)  [(1,2), (1,6), (1,7), (2,5), (2,6), ...]
  3)  Just ("I am", "Charlie")
-}

-- Assignment 6
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if (p x) then x:acc else acc) []
