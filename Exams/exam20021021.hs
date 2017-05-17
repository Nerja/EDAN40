-- Exam 2002-10-21
import Data.List
-- Assignment 1
-- Takes an argument and a list of functions and returns the results.
g :: a -> [a -> a] -> [a]
g x fs = [f x | f <- fs]

-- Assignment 2
-- zip :: [a] -> [b] -> [(a,b)]
-- Takes two lists and zips them together.
-- unzip :: [(a,b)] -> ([a],[b])
-- Takes a list of pairs and unzips them.
-- Note: ((unzip .) . zip) xs ys /= (,) xs ys

-- Assignment 3
oneOf :: Bool -> Bool -> Bool -> Bool
oneOf True False False = True
oneOf False True False = True
oneOf False False True = True
oneOf _     _     _    = False

-- Assignment 4
{-
  Type is [Char]

  do [1,2,3]; "lambda" is syntactic sugar for
  [1,2,3] >>= (\_ -> "lambda")                  ~>
  concat (map (\_ -> "lambda") [1,2,3])         ~>
  concat ["lambda", "lambda", "lambda"]         ~>
  "lambdalambdalambda"
-}

-- Assignment 5
{-
  A class declares some methods that all instances
  should implement. For example the Monad class
  defines (>>=) and the instance Maybe implements this
  function. There is also a Monad instance for lists.
  can use >>= on both lists and maybe.

  Eq is for an example good for putting constraints.
-}

-- Assignment 6
-- Set intersection
