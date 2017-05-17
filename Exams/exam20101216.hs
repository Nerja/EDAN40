-- Exam 2010-12-16

-- Assignment 1
f = (+5) . (8/)
g = (+) . (*3)

-- Assignment 2
alt f = map (f.(+4)) . filter (<5) -- answer
lc f ys = [f x | x <- [y + 4 | y <- ys, y < 5]]

-- Assignment 3
-- smallDigits is using enumeration. Add deriving (Enum) to data definition
data Digits = Zero | One | Two | Three | Four | Six | Seven | Eight | Nine
              deriving (Enum, Show)

smallDigits = [Zero .. Three]

-- Assignment 4
{-
  Pure code have no side effects. This means that the output depends only on the
  arguments. The same arguments will always produce the same output.

  Easy analysis, predictable. No I/O, no state in the common sense and only
  immutable data structures. To add element in a tree a hole new tree must be created.
-}

-- Assignment 5
-- Type is [[Char]]
-- Value is ["christmas","christmas","christmas","christmas"]

-- Assignment 6
{-
Using unification:

(a -> b) -> a -> [b] -> [b]
type error
[(b -> c) -> (a -> b) -> a -> c] -> [(b -> c) -> (a -> b) -> a -> c]
[a -> [a] -> [a]] -> [a -> [a] -> [a]]
-}
