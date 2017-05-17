-- Exam 2008-10-22
import Data.List
-- Assignment 1
{-
 map (:) :: [a] -> [[a] -> [a]]
 takes a list of things and returns a list of functions that append the
 corresponding thing.
-}

-- Assignment 2
{-
 type is [[Char]]
 rewrite to "hello" >>= (\_ -> return "world")
 using list monad
 concatMap (\_ -> return "world") "hello"
 concatMap (\_ -> ["world"]) "hello"
 concatMap (const ["world"]) "hello"
 concat [["world"], ["world"], ["world"], ["world"], ["world"]] ~>
 ["world", "world", "world", "world", "world"]
-}

-- Assignment 3
{-
  no side effects: Function output only depends on function arguments. The same
  function arguments always produces the same output.

  easy to reason about program, predictable
-}

-- Assignment 4
mapCurry :: [(a,b) -> c] -> [a -> b -> c]
mapCurry = map curry

-- Assignment 5
{-
  delete removes first occurrence of element from list
  delete :: Eq a => a -> [a] -> [a]

  (\\) = foldl (flip delete
  (\\) :: Eq a => [a] -> [a] -> [a]
  removes the first occurences of the elements in the second argument from
  the list given as the first argument.
-}
-- Returns all possible permutations of the list. The permutations are returned as
-- a list
g :: Eq a => [a] -> [[a]]
g [] = [[]]
g xs = concat [map (x:) (g (xs \\ [x])) | x <- xs]

-- Assignment 6
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if (p x) then x:acc else acc) []
