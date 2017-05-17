-- Exam 2005-10-17
import Data.List

-- Assignment 1
-- map (const (++)) :: [a] -> [[b] -> [b] -> [b]]

-- Assignment 3
iterate' :: (a -> a) -> a -> [a]
iterate' = unfoldr . g
  where g f x = Just (f x, f x) -- answer

-- Assignment 4
newtype Image a = Image (Position -> a)
type Position = (Float, Float)

type Region = Image Bool
type ColorImage = Image Color
type Color = (Int, Int, Int)

-- a
paste :: Region -> Image a -> Image a -> Image a
paste (Image reg) (Image img1) (Image img2) = Image $ \pos -> case reg pos of
                                                True -> img1 pos
                                                _    -> img2 pos

-- b
lift0 :: a -> Image a
lift1 :: (a -> b) -> Image a -> Image b
lift2 :: (a -> b -> c) -> Image a -> Image b -> Image c

lift0 x = Image $ const x
lift1 f (Image img1) = Image $ \pos -> f $ img1 pos
lift2 f (Image img1) (Image img2) = Image $ \pos -> f (img1 pos) $ img2 pos

-- c
-- Change type Image a = Position -> a to newtype Image a = Image (Position -> a)
-- Make Image an instance of Num
instance (Num a) => Num (Image a) where
  (+)         = lift2 (+)
  (-)         = lift2 (-)
  (*)         = lift2 (*)
  negate      = error "?"
  abs         = lift1 (abs)
  signum      = error "?"
  fromInteger = error "?"

-- Assignment 5
fmap' :: (Monad a, Functor a) => (b -> c) -> a b -> a c
fmap' f m = do x <- m
               return (f x)

-- extra alternative
fmap'' :: (Monad a, Functor a) => (b -> c) -> a b -> a c
fmap'' f m = m >>= return . f

-- Assignment 6
-- a)    q :: Eq a => [a] -> [a]. Remove duplicates
q :: Eq a => [a] -> [a]
q = foldr (\x acc -> x : delete x acc) []
