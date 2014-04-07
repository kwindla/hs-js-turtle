import Control.Monad.State


newtype Counter = Counter Int
instance Show Counter where
  show (Counter c) = "(c" ++ (show c) ++ ")"

lst = [11..21]

-- consume an int from the list, return Counter state obj and the int
-- transformed ... so for example -> (c24, "11(c23)")
xfrmA :: Counter -> [Int] -> (Counter, String)
xfrmA counter@(Counter c) (i:is) = (Counter (c+1), show i ++ show counter)

xfrmNthA :: Counter -> [Int] -> Int -> (Counter, String)
xfrmNthA c ints 0 = xfrmA c ints
xfrmNthA c ints@(i:is) idx = let (c', _) = xfrmA c ints
                             in xfrmNthA c' is (idx-1)


type CounterState = (Counter, String)

xfrmS :: [Int] -> State CounterState String
xfrmS (i:is) = do
  (counter@(Counter c), _) <- get
  put $ (Counter (c+1), "")  
  return $ show i ++ show counter

xfrmNthS :: [Int] -> Int -> State CounterState String
xfrmNthS ints 0 = xfrmS ints
-- xfrmNthS ints@(i:is) idx = xfrmS ints >> xfrmNthS is (idx-1)
xfrmNthS ints@(i:is) idx = do 
  xfrmS ints
  xfrmNthS is (idx-1)

startState = (Counter 23, "")

main = do
  -- print $ xfrmNthA (Counter 23) lst 2
  print $ runState (xfrmS lst) startState
  print $ runState (xfrmNthS lst 2) startState
   