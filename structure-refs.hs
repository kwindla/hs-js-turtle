

import Control.Monad.State
import Control.Arrow
import qualified Data.Map as Map


type D = ((Int, Int), (Int, Int))
newD :: D 
newD = ((0,1), (2,3))


imp :: State D Int
imp = return 34

a :: State D (Int, Int)
a = do
  f <- get
  let (f', _) = f
  return f'
  
b :: State D Int
b = do
  f <- get
  let (f', _) = f
  let (f'', _) = f'
  return f''

c :: (D -> a) -> State D a
c f = do
  s <- get
  return $ f s

d :: (D -> a) -> State D a
d f = get >>= (\a -> return $ f a)

e :: (D -> a) -> State D a
e f = state $ \s -> let (a, s') = runState (state (\t -> (t, t))) s
                    in runState ((\u -> state (\v -> (f u, v))) a) s'
                       
g :: (D -> a) -> State D a
g f = liftM f get


-- main = do
--   print $ runState ((snd >>> fst) `liftM` get) newD

--

data FRef s a = FRef
      { view :: s -> a
      , set :: a -> s -> s
      }

-- this is FRef compose ... (turtle `view'` heading) tsl
view' :: FRef a b -> FRef b c -> a -> c
view' ref ref' = (view ref' . view ref)

set' :: FRef a b -> FRef b c -> c -> a -> a
set' ref ref' a s = ((set ref . set ref' a . view ref) s) s

update :: FRef s a -> (a -> a) -> s -> s
update ref f s = set ref (f (view ref s)) s

update' :: FRef a b -> FRef b c -> (c -> c) -> a -> a
update' ref ref' f s = ((set ref . update ref' f . view ref) s) s

-- ((set turtle . set heading 77 . view turtle) newTSL) newTSL 

 
data Binding = BoundValue   Double                      
instance Show Binding where
  show (BoundValue num) = "`" ++ (show num)

type SymbolTableMapList = [(Char, Binding)]
type SymbolTableMap = (Map.Map Char Binding)

data SymbolTable = ScopedTable SymbolTableMap SymbolTable |
                   GlobalTable SymbolTableMap
  deriving (Show)

globalTable = GlobalTable $ Map.fromList 
  [ ('P', BoundValue pi)
  ]

data Turtle = Turtle { _heading :: Double
                     , _pos     :: (Double, Double)
                     , _color   :: (Int, Int, Int)
                     } deriving (Show)

data TSL = TSL { _turtle :: Turtle
               , _symTab :: SymbolTable
               , _outLines :: [String] } deriving (Show)

newTSL = TSL (Turtle 90.0 (1.0,1.0) (2,3,4)) globalTable [] 

turtle   = FRef { view = _turtle   , set = \x t -> t { _turtle = x } }
symTab   = FRef { view = _symTab   , set = \x t -> t { _symTab = x } }
outLines = FRef { view = _outLines , set = \x t -> t { _outLines = x } }
heading  = FRef { view = _heading  , set = \x t -> t { _heading = x } }
pos      = FRef { view = _pos      , set = \x t -> t { _pos = x } }
color    = FRef { view = _color    , set = \x t -> t { _color = x } }



retrBinding :: Char -> SymbolTable -> Binding
retrBinding sym (ScopedTable map parent) =
  let maybeBinding = Map.lookup sym map
  in case maybeBinding of
    Just binding -> binding
    Nothing      -> retrBinding sym parent
retrBinding sym (GlobalTable map) =
  let maybeBinding = Map.lookup sym map
  in case maybeBinding of
    Just binding -> binding
    Nothing      -> BoundValue 0

updateBinding :: Char -> Binding -> EvalContext
updateBinding sym binding = do
  st <- getST
  putST $ case st of
    (ScopedTable map parent) ->
      ScopedTable (Map.insert sym binding map) parent
    (GlobalTable map) ->
      GlobalTable $ Map.insert sym binding map
  case binding of
    (BoundValue num) -> return num
    otherwise        -> return 0


