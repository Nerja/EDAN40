import System.Random
newtype State s a = State { runState :: s -> (a,s) }

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

instance Monad (State s) where
  return = returnState
  (>>=)  = bindState

-- Takes current state and presents it as the result
get :: State s s
get = State $ \s -> (s, s)

-- Takes some state and makes a stateful function that replaces current
-- state with the given state.
put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- Task 1
type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom = do
            gen <- get
            let (val, gen') = random gen
            put gen'
            return val
