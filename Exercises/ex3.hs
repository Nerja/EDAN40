-- EDAN40
-- Ex 2.1
import qualified Data.List as Foo
import Prelude hiding (sequence_, fmap)

-- Set
newtype Set a = Set [a]

sing :: a -> Set a
sing = Set . (: [])

union :: (Ord a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set $ Foo.union xs ys

-- Laws:
-- Left identity:  return x >>= f should be the same as f x
-- ok. return x packs x in set and >>= f maps f over the set x
-- return x >>= f ~>
-- (Set [x]) >>= f ~>
-- foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) $ map f [x] ~>
-- foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) [f x] ~>
-- Set [f x]
-- Yes return x >>= f is the same as f x

-- Right identity: m >>= return should be just like m
-- m >>= return just wraps m in minimal context
-- (Set xs) >>= return ~>
-- foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) $ map return xs ~>
-- foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) [(Set x1, ..., Set xn)] ~>
-- Set xs

-- Associativity: (m >>= f) >>= g should be just like m >>= (\x -> f x >>= g)
-- ((Set xs) >>= f) >>= g ~>
-- (foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) $ map f xs) >>= g ~>
-- (Set [f x1, ..., f xn]) >>= g ~>
-- (foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) $ map g [f x1, ..., f xn]) ~>
-- (Set [g(f(x1)), ..., g(f(xn))])
--
-- (Set xs) >>= (\x -> f x >>= g) ~>
-- (foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) $ map ((\x -> f x >>= g)) xs) ~>
-- (Set [g(f(x1)), ..., g(f(xn))])
instance Monad Set where
  return            = sing
  (Set xs) >>= f    = foldr (\(Set xs) (Set ys) -> Set (xs ++ ys)) (Set []) $ map f xs

-- Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
instance Monad Tree where
  return = Leaf                                -- return :: Monad m => a -> m a. Packs a in minimal tree(leaf)
  (Leaf el) >>= f     = f el                   -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (Node l r) >>= f = Node (l >>= f) $ r >>= f  -- for leaf simply applies function to leaf value
                                               -- for node applies recursive on leaf and right.

-- Laws:
-- Left identity: return x >>= f should be the same as f x
-- return x >>= f ~>
-- Leaf x >>= f ~>
-- f x
-- ok!

-- Right identity: m >>= return should be like m
-- (Leaf el) >>= return ~>
-- return el ~>
-- Leaf el
--
-- (Node a b) >>= f ~>
-- Node (l >>= f) (r >>= f)
-- ok!

-- Associativity: (m >>= f) >>= g should be like m >>= (\x -> f x >>= g)
-- (Leaf el >>= f) >>= g ~>
-- (f el) >>= g ~>
-- g (f el)
--
-- (Leaf el) >>= (\x -> f x >>= g) ~>
-- (\x -> f x >>= g) (Leaf el) ~>
-- (f el) >>= g
-- g (f el)
--
-- ((Node l r) >>= f) >>= g ~>
-- (Node (l >>= f) (r >>= f)) >>= g
-- Node ((l >>= f) >>= g) ((r >>= f) >>= g)
--
-- (Node l r) >>= (\x -> f x >>= g) ~>
-- Node (l >>= (\x -> f x >>= g)) (r >>= (\x -> f x >>= g)) ~>
-- ok

-- Error
data Error a = OK a | Error String
instance Monad Error where
  return = OK
  (OK a) >>= f        = f a
  (Error str) >>= f   = Error str

-- Laws
-- Left identity: return x >>= f should be the same as f x
-- return x >>= f ~>
-- (OK x) >>= f   ~>
-- f x  ok!
--
-- Right identity: m >>= return should be like m
-- (OK x) >>= return ~>
-- return x ~>
-- OK x
-- (Error str) >>= return ~>
-- Error str
--
-- Good ok!

-- Associativity: (m >>= f) >>= g should be like m >>= (\x -> f x >>= g)
-- (OK x >>= f) >>= g ~>
-- (f x) >>= g ~>
-- (\x -> f x >>= g) x ~>
-- (OK x) >>= (\x -> f x >>= g)
--
-- (Error str >>= f) >>= g ~>
-- (Error str) >>= g ~>
-- Error str
--
-- (Error str) >>= (\x -> f x >>= g)
-- Error str
-- OK!

-- Ex 2.2
sequence_ :: Monad m => [m ()] -> m ()
sequence_ = foldr (>>) (return ())

onlyIf :: Monad m => Bool -> m () -> m ()
onlyIf doFlag act
  | doFlag      = act
  | otherwise   = return ()

onlyIfM :: Monad m => m Bool -> m () -> m ()
onlyIfM doFlag act = do flag <- doFlag
                        if flag
                        then act
                        else return ()

-- Ex 2.3
list2 = [1..] >>= (\x -> [1..x] >>= (\y -> [(x,y)]))
-- can also use return (x,y) will pack (x,y) into [(x,y)]
list2' = [1..] >>= (\x -> [1..x] >>= (\y -> return (x,y)))

-- Ex 2.4
-- Task 1
{-
Need to prove:
return >@> f = f
\x -> (return x) >>= f = f
Can also prove:
(return x) >>= f = f x

Need to prove:
f >@> return = f
\x -> (f x) >>= return = f
Can also prove:
(f x) >>= return = f x
or
m >>= return = m

Need to prove:
(f >@> g) >@> h = f >@> (g >@> h)
Can also prove:
(m >>= f) >>= g = m >>= \x -> (f x >>= g)

For Id
------------------------------------------------------------
(return x) >>= f ~>
(Id x) >>= f ~>
f x
ok!

(f x) >>= return ~>
return (f x) ~>
Id (f x) ~>
f x
ok!

(m >>= f) >>= g ~>
((Id x) >>= f) >>= g ~>
(f x) >>= g

m >>= \x -> (f x >>= g) ~>
(Id x) >>= \x -> (f x >>= g) ~>
(\x -> (f x >>= g)) x ~>
f x >>= g
ok both are the same :)

For list []
------------------------------------------------------------
(return x) >>= f ~>
[x] >>= f ~>
concat (map f [x]) ~>
concat [f x] ~>
f x
ok!

m >>= return = m ~>
concat (map return [x1, ..., xn]) ~>
concat ([return x1, ..., return xn]) ~>
concat ([[x1], ..., [xn]]) ~>
[x1, ..., xn] ~>
m
ok!

(m >>= f) >>= g ~>
(concat (map f m)) >>= g ~>
concat (map g ((concat (map f [x1, ..., xn])))) ~>
concat (map g [f x1, ..., f xn]) ~>
concat [[g(f(x1))], ..., [g(f(xn))]] ~>
[g(f(x1)), ..., g(f(xn))]

m >>= \x -> (f x >>= g) ~>
m >>= \x -> (concat (map g (f x))) ~>
concat (map (\x -> (concat (map g (f x)))) m) ~>
concat (map (\x -> (concat (map g (f x)))) [x1, ... xn]) ~≳
concat [[g(f(x1)), ..., g(f(xn))]] ~>
[g(f(x1)), ..., g(f(xn))]
ok same :)

For Maybe
------------------------------------------------------------
(return x) >>= f ~>
Just x >>= f ~>
f x
Ok!

(f x) >>= return = f x

Just x >>= return ~>
return x ~>
Just x

Nothing >>= return ~>
Nothing
Ok!

Om Nothing:
(Nothing >>= f) >>= g ~>
Nothing >>= g ~>
Nothing

Nothing >>= \x -> (f x >>= g) ~>
Nothing

Om Just x
(Just x >>= f) >>= g ~>
(f x) >>= g

Just x >>= \x -> (f x >>= g) ~>
(\x -> (f x >>= g)) x ~>
f x >>= g
Ok same in all cases!

-- Task 2
Want to prove fmap (f . g) = fmap f . fmap g
or
fmap (f . g) m = (fmap f . fmap g) m

fmap (f . g) m ~>
do x <- m
   return ((f . g) x)

(fmap f . fmap g) m ~>
fmap f (fmap g m) ~>
fmap f (do x <- m
           return (f x)) ~>
do x <- return (f x) ~>
   return (g ( f (x)))

same! ok

-- Task 3
join return = join . fmap return
join return m = (join . fmap return) m

join return m ~>
do x <- return m
   x
~> m

(join . fmap return) m ~>
do x <- fmap return m
   y <- join x
   y

do x <- return (return m)
   y <- join (return m)
   y

~> m
-}

-- Task 4
