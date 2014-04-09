
import Control.Monad.State


testStartState = Counter 101
testList = [1..10]

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


-- rewrite get and put to do the manipulations ourselves so that we
-- know how to write more sophisticated (and encapsulating) functions
-- that match up with the State Monad's assumptions, should we so
-- desire

get' :: State Counter Counter
get' = state (\c -> (c ,c)) 

put' :: Counter -> State Counter ()
put' c = state (\_ -> ((), c))
  
xfrmS' :: [Int] -> State Counter String
xfrmS' (i:is) = do
  counter@(Counter c) <- get'
  put' $ Counter (c+1)
  return $ show i ++ show counter

xfrmNthS' :: [Int] -> Int -> State Counter String
xfrmNthS' ints 0 = xfrmS' ints
xfrmNthS' ints@(i:is) idx = do 
  xfrmS' ints
  xfrmNthS' is (idx-1)


--
 
main = do
  print $ xfrmNth testStartState testList 2
  print $ runState (xfrmNthS testList 2) testStartState
  print $ runState (xfrmNthSOps testList 2) testStartState
  print $ runState (xfrmNthS' testList 2) testStartState
   
  -- print $ runState (xfrmS testList) testStartState



{-

-- and for further head-scratching, try combining mapM with our above
-- exercise. hmph. can't figure out how to simple "replace" the
-- recursive bits of xfrmNth with a mapM (or similar) call.
-- my understanding of the Monad[Transformer] stuff obviously is
-- still very, very incompletel


xfrmSLoop :: Int -> State Counter String
xfrmSLoop i = do
  counter@(Counter c) <- get
  put $ Counter (c+1)
  return $ show i ++ show counter
  -- show i ++ show counter

*Main> runState (mapM xfrmSLoop [1,2,3]) (Counter 0)
(["1(c0)","2(c1)","3(c2)"],(c3))

-}

--
