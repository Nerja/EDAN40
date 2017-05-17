fmap' :: (Monad m) => (a -> b) -> m a -> m b
fmap' f x = do a <- x
               return $ f a

fmap'' :: (Monad m) => (a -> b) -> m a -> m b
fmap''  f x = x >>= (\a -> return $ f a)

-- Ass 6
data Tree = Leaf String
          | Node String Tree Tree
            deriving (Show, Eq)

subTree :: Tree -> Tree -> Bool
subTree (Leaf x) (Leaf y)               = x == y
subTree a@(Leaf x) (Node y ly ry)       = x == y || subTree a ly || subTree a ry
subTree (Node _ _ _) (Leaf _)           = False
subTree a@(Node x lx rx) (Node y ly ry)
  | x /= y    = subTree a ly || subTree a ry
  | otherwise = subTree a ly || subTree a ry || lx == ly && rx == ry

t1 = Leaf "Hej"
t2 = Node "Hej" (Leaf "k") (Leaf "a")
t3 = Node "hehe" (Leaf "Hej") (Leaf "Hej")
t4 = Node "5" (Leaf "6") (Leaf "9")
t5 = Node "1337" (Node "5" (Node "5" (Leaf "6") (Leaf "9")) (Leaf "777")) (Node "9" (Leaf "1") (Leaf "1"))
t6 = Node "1337" (Node "5" (Node "5" (Leaf "6HEHE") (Leaf "9")) (Leaf "777")) (Node "9" (Leaf "1") (Leaf "1"))
