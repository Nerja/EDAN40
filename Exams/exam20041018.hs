-- Exam 2004-10-18

-- Assignment 1
test :: Bool -> Bool -> Bool -> Bool
test True True c = not c
test a True True = not a
test True b True = not b
test _ _ _       = False

-- Assignment 2
-- Alt 1)
g f = \x -> f ((f x)/3)

-- Alt 2)
g' f = f . (/3) . f

-- Assignment 3
e k = do x <- k
         return (2*x)
         return False

-- Rewrite to k >>= (\x -> return (2*x) >>= (\_ -> return False))
-- easy to see that type is (Monad m, Num a) => m a -> m Bool

-- Assignment 4
-- Unify Behavior a with Behavior Color. Gives a = Color
-- The second param to switch will unify with Event (Behavior Color). This
-- gives the type col :: Behavior Color.

-- Assignment 5
-- A type that is an instance of the Monad class.
-- From lecture:  1) Seperation of pure and impure code
--                2) Properties of a particular kind of functions
--                3) Introduction of state and its transformations.


-- Assignment 6
-- We need to able to reverse on step
-- so it must hold that f' (f x y) = Just (x, y) then it is possible
-- to extract x y.
--
-- It must also be the case that f' z = Nothing since we need to stop
-- the extraction at some point
