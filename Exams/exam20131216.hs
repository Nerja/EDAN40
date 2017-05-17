-- Ass 1
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f s [] = []
scanr' f s (x:xs)
  | null rec      = f x s : s : []
  | otherwise     = f x (head rec) : rec
  where rec = scanr' f s xs

scanr'' :: (a -> b -> b) -> b -> [a] -> [b]
scanr'' f s = foldr insVal [s]
  where insVal x acc = f x (head acc) : acc

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ _ [] = []
scanl' f s (x:xs) = apply : rec
  where apply = f s x
        rec   = scanl' f apply xs

scanl'' :: (a -> b -> a) -> a -> [b] -> [a]
scanl'' f s = foldl insVal [s]
  where insVal acc x = acc ++ [f (last acc) x]

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

type ChurchNatural a = (a -> a) -> (a -> a)
zeroC, oneC, twoC :: ChurchNatural a
zeroC f = id
oneC f = f
twoC f = f.f
succC n f = f.(n f)
threeC = succC twoC
fourC = succC threeC
plusC x y f = (x f) . (y f)

data Music = Note String Int [Int]
           | Rest Int
           | Music :+: Music
           | Music :=: Music
           deriving (Show)

line = foldr (:+:) (Rest 0)
lineToList n@(Rest 0) = []
lineToList (n :+: ns) = n : lineToList ns


line2 :: [Music] -> Music
line2 [] = Rest 0
line2 (n@(Note _ _ _ ) : ms) = n :+: line2 ms
line2 (r@(Rest _) : ms) = r :+: line2 ms
line2 ((m1:+:m2) : ms) = m1 :+: (m2 :+: line2 ms)
line2 ((m1:=:m2) : ms) = (m1 :=: m2) :+: line2 ms

lineToList2 :: Music -> [Music]
lineToList2 (n@(Note _ _ _)) = [n]
lineToList2 (r@(Rest _)) = [r]
lineToList2 (m1:+:m2) = lineToList2 m1 ++ lineToList2 m2
lineToList2 (m1:=:m2) = lineToList2 m1 ++ lineToList2 m2
