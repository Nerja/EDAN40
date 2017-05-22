import Data.List
-- Error
data Error a = OK a | Error String
instance Monad Error where
  return = OK
  (OK a) >>= f        = f a
  (Error str) >>= f   = Error str

{-
Left identity return x >>= f == f x
return x >>= f ~>
(OK x) >>= f   ~>
f x
ok!

Right identity: m >>= return should be like m
(OK x) >>= return ~>
return x ~>
OK x

(Error str) >>= return ~>
Error str
ok!

Associativity: (m >>= f) >>= g should be like m >>= (\x -> f x >>= g)
(OK x >>= f) >>= g  ~>
(f x) >>= g         ~>
(\x -> f x >>= g) x ~>
(OK x) >>= (\x -> f x >>= g)

(Error s >>= f) >>= g ~>
(Error s) >>= g       ~>
Error s

(Error s) >>= (\x -> f x >>= g) ~>
Error s
ok!
-}

-- Set
newtype Set a = Set [a]

empty :: Set a
empty = Set []

combine :: (Eq a) => Set a -> Set a -> Set a
combine (Set xs) (Set ys) = Set $ xs `union` ys

instance Monad Set where
  return x = Set [x]
  (Set xs) >>= f = unionMult (map f xs)

unionMult :: [Set a] -> Set a
unionMult = Set . concatMap (\(Set xs) -> xs)

{-
  Left identity: return x >>= f == f x
  return x >>= f        ~>
  Set [x] >>= f         ~>
  unionMult (map f [x]) ~>
  unionMult ([f x])     ~>
  f x
  ok!

  Right identity: m >>= return == m
  (Set xs) >>= return                   ~>
  unionMult (map return xs)             ~>
  unionMult ([Set [x1], ..., Set [xn]]) ~>
  Set (concatMap [[x1], ..., [xn]])     ~>
  Set [x1, ..., xn]                     ~>
  Set xs
  ok!
-}

-- Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving (Show, Eq)

instance Monad Tree where
  return = Leaf
  (Leaf el) >>= f   = f el
  (Node l r) >>= f  = Node (l >>= f) (r >>= f)

{-
Left identity: return x >>= f == f x
return x >>= f    ~>
Leaf x >>= f      ~>
f x
ok!

Right identity: m >>= return == m
m = Leaf x
Leaf x >>= return ~>
return x          ~≳
Leaf x

m = Node l r
Node l r >>= return                ~>
Node (l >>= return) (r >>= return) ~> does not affect leafs and node just applies recursively
Node l r
ok!

Associativity: (m >>= f) >>= g == m >>= (\x -> f x >>= g)
m = Leaf x
(Leaf x >>= f) >>= g ~>
(f x) >>= g          ~>

(Leaf x) >>= (\x -> f x >>= g) ~>
(\x -> f x >>= g) x ~>
f x >>= g

m = Node l r
((Node l r) >>= f) >>= g ~>
(Node (l >>= f) (r >>= f)) >>= g
Node ((l >>= f) >>= g) ((r >>= f) >>= g)

(Node l r) >>= (\x -> f x >>= g) ~>
Node (l >>= (\x -> f x >>= g)) (r >>= (\x -> f x >>= g))

If l = Leaf x, r = Leaf y:
Node ((Leaf x >>= f) >>= g) ((Leaf y >>= f) >>= g) ~>
Node (f x >>= g) (f y >>= g)

Node (Leaf x >>= (\x -> f x >>= g)) (Leaf y >>= (\x -> f x >>= g)) ~>
Node ((\x -> f x >>= g) x) ((\x -> f x >>= g) y) ~>
Node (f x >>= g) (f y >>= g)
Same repeat for other cases ok.
-}
