module SymbolTable
       where

import ExprTree
import Control.Monad.State
import qualified Data.Map as Map


-- smells like this should not be here ... bindings shouldn't return
-- EvalContext?
type EvalContext = State TSL Double

data Binding = BoundValue   Double                           |
               BoundBuiltin Int ([ExprTree] -> EvalContext)  | 
               BoundDefun   Int ExprTree SymbolTable

instance Show Binding where
  show (BoundValue num) = "`" ++ (show num)
  show (BoundBuiltin arity _) = "#" ++ (show arity)
  show (BoundDefun arity _ _) = "&" ++ (show arity)

type SymbolTableMapList = [(Char, Binding)]
type SymbolTableMap = (Map.Map Char Binding)

data SymbolTable = SymbolTable SymbolTableMap
  deriving (Show)

data Turtle = Turtle { _heading :: Double
                     , _pos     :: (Double, Double)
                     , _color   :: (Int, Int, Int)
                     } deriving (Show)

data TSL = TSL { _turtle :: Turtle
               , _symTab :: SymbolTable
               , _outLines :: [String] } deriving (Show)

turtle   = FRef { view = _turtle   , set = \x t -> t { _turtle = x } }
symTab   = FRef { view = _symTab   , set = \x t -> t { _symTab = x } }
outLines = FRef { view = _outLines , set = \x t -> t { _outLines = x } }
heading  = FRef { view = _heading  , set = \x t -> t { _heading = x } }
pos      = FRef { view = _pos      , set = \x t -> t { _pos = x } }
color    = FRef { view = _color    , set = \x t -> t { _color = x } }

getST = (gets $ view symTab) ::State TSL SymbolTable
putST st = modify $ set symTab st  
getBinding sym = gets $ retrBinding sym . view symTab
appendOutput lines = modify $ update outLines (++[lines])    
  
updateBinding sym binding = do
  (SymbolTable map) <- getST
  putST $ SymbolTable (Map.insert sym binding map)
  case binding of
    BoundValue num -> return num
    otherwise -> return 0.0

  -- putST $ SymbolTable (Map.insert sym binding map) parent
  -- case binding of
  --   BoundValue num -> return num
  --   otherwise -> return 0.0


derivedTable :: SymbolTableMapList -> SymbolTable -> SymbolTable
derivedTable list (SymbolTable parentMap) =
  SymbolTable $ Map.union (Map.fromList list) parentMap
  
-- we have a bit of static scope machinery we have to put in place,
-- here. the rule is that at defun time we save the current
-- SymbolTable context. that gives us (up the table chain) a full list
-- of every previously defined variable that we're closing over. but
-- these variables could have been modified after the defun. to
-- account for that, we need to set all our variables to their current
-- values in the symbol table of our caller (the "dynamic" SymbolTable
-- arg, below). this works because it's impossible for our function
-- defun to be visible from any scope other than one directly
-- descending from the calling scope (we don't have first-class
-- functions) AND we don't have local variable re-definition (only
-- changing of values).
funcScopedTable :: SymbolTableMapList -> SymbolTable -> SymbolTable -> SymbolTable
funcScopedTable list s@(SymbolTable parentMap) dynamicTable =
  let updatedMap = Map.mapWithKey f parentMap
  in SymbolTable $ Map.union (Map.fromList list) updatedMap
     where f k v =
             case retrBinding k dynamicTable of
               b@(BoundValue _)     -> b
               b@(BoundBuiltin _ _) -> b
               otherwise            -> v

retrBinding :: Char -> SymbolTable -> Binding
retrBinding sym (SymbolTable map) =
  let maybeBinding = Map.lookup sym map
  in case maybeBinding of
    Just binding -> binding
    Nothing      -> BoundValue 0.0
  
  -- let maybeBinding = Map.lookup sym map
  -- in case maybeBinding of
  --   Just binding -> binding
  --   Nothing      -> case parent of
  --                        Just st@(SymbolTable _ _) -> retrBinding sym st
  --                        Nothing -> BoundValue 0.0


--


-- Functional references. See (for example):
-- http://twanvl.nl/blog/haskell/overloading-functional-references We
-- could use Data.Lens or any of several other libraries. Nice to
-- stick to Haskell 98, though, to maximize chances we can
-- cross-compile this to javascript. Also it's fun to define these by
-- hand here.

data FRef s a = FRef
      { view :: s -> a
      , set :: a -> s -> s
      }

view' :: FRef a b -> FRef b c -> a -> c
view' ref ref' = (view ref' . view ref) -- lifted compose, you know

set' :: FRef a b -> FRef b c -> c -> a -> a
set' ref ref' a s = ((set ref . set ref' a . view ref) s) s

update :: FRef s a -> (a -> a) -> s -> s
update ref f s = set ref (f (view ref s)) s

update' :: FRef a b -> FRef b c -> (c -> c) -> a -> a
update' ref ref' f s = ((set ref . update ref' f . view ref) s) s
