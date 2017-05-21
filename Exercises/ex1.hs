import Data.List
-- EDAN40
-- 2.1.1
maxi :: (Ord a) => a -> a -> a
maxi a b
  | b > a     = b
  | otherwise = a

-- 2.1.2
sumsq :: Integer -> Integer
sumsq = foldr ((+).(^2)) 0 . enumFromTo 1

-- 2.1.3
hanoi :: Integer -> Integer
hanoi n
  | n <= 0    = 0
  | otherwise = 1 + 2 * hanoi (n-1)

-- 2.1.4
--nextFactor k n which returns the smallest factor of n larger than k
nextFactor :: Integer -> Integer -> Integer
nextFactor k n
  | k >= n              = n
  | n `mod` (k+1) == 0  = k+1
  | otherwise           = nextFactor (k+1) n

smallestFactor :: Integer -> Integer
smallestFactor = nextFactor 1

numFactors :: Integer -> Integer
numFactors n = toInteger $ length $ nub $ map (flip nextFactor n) [1..n]

isPrime :: Integer -> Bool
isPrime = (<=1). numFactors

-- 2.1.5
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dev
             deriving (Show, Eq, Read, Ord, Enum, Bounded)

normYear = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
daysInMonth :: Month -> Integer -> Integer
daysInMonth Feb y = if (y `mod` 4 == 0) then 28 else 29
daysInMonth m _   = normYear !! fromEnum m

data Date = Date Integer Month Integer
            deriving (Show, Eq, Read, Ord)

validDate :: Date -> Bool
validDate (Date y m d) = d >= 1 && (d <= daysInMonth m y)



-- 2.2.1
product' :: (Num a) => [a] -> a
product' = foldr (*) 1

-- 2.2.2
substitute :: (Eq a) => a -> a -> [a] -> [a]
substitute f t = map sel
  where sel el | el == f   = t
               | otherwise = el

-- 2.2.3
duplicates :: (Eq a) => [a] -> Bool
duplicates xs = removeDuplicates xs /= xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl addUnique []
  where addUnique acc x | x `elem` acc  = acc
                        | otherwise     = acc ++ [x]

-- 2.2.4 Define a function of n that will find all Pythagorean triads with a<=b<=c<=n.
pythagorean n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a <= b, b <= c, c <= n, a^2 + b^2 == c^2]

-- 2.2.5
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _  = False
isPermutation (x:xs) ys
  | x `elem` ys = isPermutation xs $ delete x ys
  | otherwise   = False

-- 2.2.6
shortestAndLongest :: [String] -> (String, String)
shortestAndLongest [] = ("", "")
shortestAndLongest xs = foldr sel (last xs, last xs) xs
  where sel x acc = (shortest x $ fst acc, longest x $ snd acc)
        longest x cl | length x > length cl = x
                     | otherwise            = cl
        shortest x cs | length x < length cs = x
                      | otherwise           = cs

-- 2.2.7 quickCheck (\xs -> xs == mystery xs) gives back list
mystery xs = foldr (++) [] (map (\y -> [y]) xs)
