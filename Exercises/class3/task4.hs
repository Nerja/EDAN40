{-
(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >@> g = \x -> (f x) >>= g
Kleisli composition

M1) return >@> f = f
M2) f >@> return = f
M3) (f >@> g) >@> h = f >@> (g >@> h)

Can also prove
M1) \x -> (return x) >>= f == f  or  return x >>= f == f x
M2) \x -> (f x) >>= return == f x or f x >>= return == f x
M3) \x -> ((f >@> g) x) >>= h == \x -> (f x) >>= (g >@> h) or
    ((f >@> g) x) >>= h == (f x) >>= (g >@> h)
    ((\x -> f x >>= g) x) >>= h == (f x) >>= (\x -> g x >>= h)
    (f x >>= g) >>= h == (f x) >>= (\x -> g x >>= h)

------------- Id --------------
instance Monad Id where
  return          = Id
  (Id x) >>= f    = f x

M1)
return x >>= f    ~>
Id x >>= f        ~>
f x

M2)
f x >>= return    ~> f x == Id y
Id y >>= return   ~>
Id y              ~>
f x

M3)
(f x >>= g) >>= h             ~> f x == Id y
(Id y >>= g) >>= h            ~>
(g y) >>= h                   ~>
(\x -> g x >>= h) y           ~>
(Id y) >>= (\x -> g x >>= h)  ~> Id y ==  f x
(f x) >>= (\x -> g x >>= h)

------------- [] --------------
instance Monad [] where
  return x      = [x]
  xs >>= f      = concat (map f xs)

M1)
return x >>= f      ~>
[x] >>= f           ~>
concat (map f [x])  ~>
concat [f x]        ~>
f x

M2)
f x >>= return          ~> f x == ys
concat (map return ys)  ~>
concat [[y1], ... [yn]] ~>
[y1, ..., yn]           ~>
ys                      ~> ys == f x
f x

M3)
(f x >>= g) >>= h                             ~> f x == ys
(ys >>= g) >>= h                              ~>
(concat (map g ys)) >>= h                     ~>
concat (map h ((concat (map g ys))))          ~>
concat (map h ((concat ([g y1, ..., g yn])))) ~>
concat (map h (g y1 ++ ... ++ g yn))          ~>

(f x) >>= (\x -> g x >>= h)                   ~> f x == ys
ys >>= (\x -> g x >>= h)                      ~>
concat (map ((\x -> g x >>= h)) ys)           ~>
concat ([g y1 >>= h, ..., g yn >>= h])        ~>
concat (map h (g y1 ++ ... ++ g yn))

------------- Maybe --------------
instance Monad Maybe where
  return            = Just
  (Just x) >>= f    = f x
  Nothing >>= f     = Nothing

M1)
return x >>= f        ~>
Just x >>= f          ~>
f x

M2)
f x >>= return        ~> f x == Just y
Just y >>= return     ~>
return y              ~>
Just y                ~> Just y == f x
f x

f x >>= return        ~> f x == Nothing
Nothing >>= return    ~>
Nothing               ~> Nothing == f x
f x

M3)
f x == Just y
--
(Just y >>= g) >>= h        ~>
(g y) >>= h

Just y >>= (\x -> g x >>= h)  ~>
g y >>= h
--
f x == Nothing
(Nothing >>= g) >>= h     ~>
Nothing >>= h             ~>
Nothing

Nothing >>= (\x -> g x >>= h) ~>
Nothing

----------------------------------
Proven:
M1) \x -> (return x) >>= f == f  or  return x >>= f == f x
M2) \x -> (f x) >>= return == f x or f x >>= return == f x
M3) \x -> ((f >@> g) x) >>= h == \x -> (f x) >>= (g >@> h) or
    ((f >@> g) x) >>= h == (f x) >>= (g >@> h)
    ((\x -> f x >>= g) x) >>= h == (f x) >>= (\x -> g x >>= h)
    (f x >>= g) >>= h == (f x) >>= (\x -> g x >>= h)

fmap f m = m >>= f >>=

M4)     fmap (f . g) == fmap f . fmap g
Prove:  fmap (f . g) m == (fmap f . fmap g) m
fmap (f . g) m ~>
return ((f . g) m) ~>
return (f (g m))

(fmap f . fmap g) m ~>
(fmap f (fmap g m)) ~>
fmap f (return (g m)) ~>
return (f (g m))

join return x == (join . fmap return) x

join (return m) ~>
m

(join . fmap return) x ~>
join (fmap return x)   ~>
join (return m) ~>
m

join return m == id m
join (return m) ~>
m

id m ~>
m

-}
