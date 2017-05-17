-- Exam 2015-01-13

-- Assignment 1
f = (/3) . (5-)
g = (/) . (5-)

-- Assignment 2
{-
  f :: (Monad m, Num a) => m a -> m a -> m a
  value of f [1,2,3] [2,4,8] is [2,4,8,4,8,16,6,12,24]
  value of f (Just 5) Nothing is Nothing
-}

-- Assignment 3
{-
 It is possible to do partial function application.
-}

-- Assignment 4
-- x must then be Enum so does not work in the general case!
replicate' :: Enum a => Int -> a -> [a]
replicate' n x = take n [x,x..x]

-- Assignment 5
{-
zipWith map :: [a -> b] -> [[a]] -> [[b]]
Takes a list of functions (a->b) and applies each function to
a corresponding given list. Returns the results

map zipWith :: [a -> b -> c] -> [[a] -> [b] -> [c]]

map.zipWith :: (a -> b -> c) -> [[a]] -> [[b] -> [c]]
-}

-- Assignment 6
{-
  foldl: Folding from left -> right
  foldr: Folding from left <- right

  foldl :: (a -> b -> a) -> a -> [b] -> a
  foldr :: (a -> b -> b) -> b -> [a] -> b
-}

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).(f$)) []
