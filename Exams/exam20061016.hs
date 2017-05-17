-- Exam 2006-10-16

-- Assignment 1
f :: Eq a => [a] -> [a] -> [a]
f = filter . flip elem

f' :: Eq a => [a] -> [a] -> [a]
f' s t = [e | e <- t, e `elem` s]


-- Assignment 2
{-
  In Haskell all functions are curried in the sense that all functions
  take one parameter. This is great since it is possible to do partial function
  application.
-}

-- Assignment 3
e k = do x <- k
         Nothing
         return False

-- type will be :: Maybe a -> Maybe Bool
-- value will be Nothing in all cases because of the second row.

-- Assignment 4
{-
a) Num a => a -> a
b) Num a => a -> a
c) (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
d) a -> [a]
e) (a -> b) -> a -> b
f) Ord a => [a -> a -> Bool]
-}

-- Assignment 5
data Music = Note String Int [Int]
           | Rest Int
           | Music :+: Music
           | Music :=: Music
           deriving (Show)

line = foldr (:+:) (Rest 0) :: [Music] -> Music
lineToList :: Music -> [Music]
lineToList n@(Rest 0) = []
lineToList (n :+: ns) = n : lineToList ns
m1 = [Note "C5" 1 [] :+: Note "D5" 2 [], Note "E5" 3 []]
m2 = [Note "C5" 1 [], Note "D5" 2 [], Note "E5" 3 []]
--a) The nesting is different
--b)
line2 :: [Music] -> Music
line2 (n@(Note _ _ _) : ms) = n :+: line2 ms
line2 (r@(Rest _) : ms) = r :+: line2 ms
line2 ((m1 :+: m2) : ms) = m1 :+: (m2 :+: line2 ms)
line2 (p@(m1 :=: m2) : ms) = (m1 :=: m2) :+: line2 ms
line2 [] = Rest 0

--c)
lineToList2 :: Music -> [Music]
lineToList2 n@(Note _ _ _) = [n]
lineToList2 r@(Rest _) = [r]
lineToList2 (m1 :+: m2) = lineToList2 m1 ++ lineToList2 m2
lineToList2 (m1 :=: m2) = lineToList2 m1 ++ lineToList2 m2

-- Assignment 6
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).(f$)) []
