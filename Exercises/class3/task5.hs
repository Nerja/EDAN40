import System.Random
-- Note that runState function will have type  State s a -> s -> (a, s).
newtype State s a = State { runState :: s -> (a, s) }

-- Takes a value and reports it as the result without changing state
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s')   = runState m s
                                  (State g) = k a
                              in  g s'

instance Monad (State s) where
  return  = returnState
  (>>=)   = bindState

-- Takes a state and presents it as the result and the new state
get :: State s s
get = State $ \s -> (s, s)

-- Replaces current state with given state
put :: s -> State s ()
put s = State $ const ((), s)

-- Use for creating random numbers
{-
  Looking at random number generation we see that we will get both a random number
  and a new generator back.
-}

randomExample :: (RandomGen g) => g -> (Int, g)
randomExample gen = random gen

-- Using fmap randomExample getStdGen we get a "random" number.
-- Now for some more random numbers

funny :: (RandomGen g) => g -> ((Int, Int, Int), g)
funny gen = let (x, gen') = random gen
                (y, gen'') = random gen'
                (z, gen''') = random gen''
            in ((x,y,z), gen''')

{-
  This works but using State monad we can let the random generator
  be the state and it should be possible to do the state propagation in
  a nice way.
-}
type RandomState a = State StdGen a

randomSt :: RandomState Int
randomSt = State random

cleanFunny :: RandomState (Int, Int, Int)
cleanFunny = do a <- randomSt
                b <- randomSt
                c <- randomSt
                return (a,b,c)

-- Desugar of cleanFunny shows how the state passing of the generator is
-- hidden from us.
cleanFunny' :: RandomState (Int, Int, Int)
cleanFunny' = randomSt >>= (\a -> randomSt >>= (\b -> randomSt >>= (\c -> return (a,b,c))))
