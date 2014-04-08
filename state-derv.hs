
import Control.Monad.State


startState = Counter 23
testList = [11..21]

-- data definition for the stateful Counter that we'll use throughout

newtype Counter = Counter Int
instance Show Counter where
  show (Counter c) = "(c" ++ (show c) ++ ")"

-- baseline functions that "thread state" through the computation by hand
--
-- xfrm. given a list of ints, returns the
-- first one stringified to attach a counter value from our stateful
-- counter and also returns an incremented version of the counter
--   so ... xfrm (Counter 1) [1] = ("1(c1)", Counter 2)
xfrm :: Counter -> [Int] -> (String, Counter)
xfrm counter@(Counter c) (i:is) = (show i ++ show counter, Counter (c+1))

-- xfrmNth. walks a list recursively, updating the counter, finally
-- returning the "xfrm"ed value of the requested index
--  so ... xfrmNth (Counter 1) [1,2,3] 2 = ("3(c3)", Counter 4)
xfrmNth :: Counter -> [Int] -> Int -> (String, Counter)
xfrmNth c ints 0 = xfrm c ints
xfrmNth c ints@(i:is) idx = let (_, c') = xfrm c ints
                            in xfrmNth c' is (idx-1)


-- simple State Monad versions of the above using the get and put
-- functions defined by Control.Monad.State
--
-- so ... let s = xfrmNthS [1,2,3] 2
--        runState s (Counter 1) = ("3(c3"), Counter 4)

xfrmS :: [Int] -> State Counter String
xfrmS (i:is) = do
  counter@(Counter c) <- get
  put $ Counter (c+1)
  return $ show i ++ show counter

xfrmNthS :: [Int] -> Int -> State Counter String
xfrmNthS ints 0 = xfrmS ints
xfrmNthS ints@(i:is) idx = do 
  xfrmS ints
  xfrmNthS is (idx-1)


-- desugar the do notation and use >>= and >> operators. nesting and
-- types are a bit tricky.
  
xfrmSOps :: [Int] -> State Counter String
xfrmSOps (i:is) =
  get >>= (\a -> let counter@(Counter c) = a
                 in put (Counter (c+1)) >>
                    return (show i ++ show counter)
          )

xfrmNthSOps :: [Int] -> Int -> State Counter String
xfrmNthSOps ints 0 = xfrmS ints
xfrmNthSOps ints@(i:is) idx = xfrmSOps ints >> xfrmNthSOps is (idx-1)


{-

type MyState = Int
 
valFromState :: MyState -> Int
valFromState s = -s
nextState :: MyState->MyState
nextState x = 1+x
 
type MyStateMonad  = State MyState 
 
-- this is it, the State transformation.  Add 1 to the state, return -1*the state as the computed value.
getNext ::   MyStateMonad  Int
getNext  = state  (\st -> let st' = nextState(st) in (valFromState(st'),st') )

-}



main = do
  -- print $ xfrmNthA (Counter 23) testList 2
  print $ runState (xfrmS testList) startState
  print $ runState (xfrmNthS testList 2) startState
   