-- Exam 2010-10-20
import Control.Applicative
-- Assignment 1
applyPlusOne f = (+1) . f

-- Assignment 2
-- 1) [x,x..] should be written as [x,x..x]
-- 2) This uses enumeration so replicate :: Enum a => Int -> a -> [a].
--    Does not work for the general case
replicate' :: Enum a => Int -> a -> [a]
replicate' n x = take n [x,x..x]

-- Assignment 3
h f = fst . head . dropWhile (uncurry (/=)) . ps (iterate f)
 where
 ps g x = zip (tail (g x)) (g x)

-- Some unification gives: h :: (Eq a) => (a -> a) -> a -> a
-- Fixed point computation

-- Assignment 4
lookup' :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup' key = foldl (f key) Nothing

-- Defined function f, i defined to Nothing
f :: Eq a => a -> Maybe b -> (a,b) -> Maybe b
f _ (Just y) _ = Just y
f k _ (x,y)    = case k==x of
                    True -> Just y
                    _    -> Nothing

-- Assignment 5
f1 :: ((a,b) -> c) -> b -> a -> c
f1 f = flip (curry f)

f2 :: ((a,b) -> c) -> b -> a -> c
f2 f = curry (f.swap)
  where swap :: (a,b) -> (b,a)
        swap (x,y) = (y,x)

-- Assignment 6
-- Make Maybe an instance of Num
instance (Num a) => Num (Maybe a) where
  a + b = (+) <$> a <*> b
  a - b = (-) <$> a <*> b
  a * b = (*) <$> a <*> b
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = Just . fromInteger
