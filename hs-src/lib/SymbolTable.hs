module SymbolTable
       where

import ExprTree
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Control.Arrow

-- FIX: this file should not be called SymbolTable - or it should be
-- split into multiple files. Can't the eval context stuff be in the
-- Evaluator module? 

-- FIX: smells like this should not be here ... bindings shouldn't
-- return EvalContext?
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

data SymbolTable = SymbolTable SymbolTableMap (Maybe SymbolTable)
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
    

appendOutput lines = modify $ update outLines (++[lines])    


getST = (gets $ view symTab) ::State TSL SymbolTable
putST st = do { s <- get ; put $! set symTab st s }
getBinding sym = gets $ retrBinding sym . view symTab
updateBinding sym binding = do
  st <- getST
  putST $ setBinding st sym binding 
  case binding of
    BoundValue num -> return num
    otherwise -> return 0.0

derivedTable :: SymbolTableMapList -> SymbolTable -> SymbolTable
derivedTable list parent =
  SymbolTable (Map.fromList list) (Just parent)
  
-- funcScopedTable makes a lexically scoped symbol table, with args
-- and "write barrier" definitions installed in the last frame, ready
-- to be used within a function execution context.
--
-- the symbol table from the calling scope is the new st's parent. on
-- return from the function, the evaluator needs only to "pop" this
-- new st frame to get rid of both args and locals. the challenge here
-- is handling variables that were defined in the enclosing lexical
-- scope of the function's defun. we want to treat those as closed
-- over. because we don't have first class functions, we can use a
-- very simple model: for closed over bindings we can just use the
-- symbol table from the calling scope. on the other hand, we want to
-- treat all variables not used prior to the defun as local to the
-- function's lexical scope. the write barrier bindings hide any
-- bindings from the caller's scope that were first defined after the
-- function defun. in other words, any values in the symbol table from
-- the calling scope that are *not* in the symbol table from the defun
-- scope should be hidden by new (BoundValue 0.0)
-- 
-- so ...  (SymbolTable <locals + write barriers> callingScopeSymbolTable)
--
-- examples
--
-- &f0{q=100} q=23 f q      -- here, when we call f we will need to hide
--                             the q=23 binding so that the final q
--                             evaluates to 23

-- q=10 &f0{q=100} q=23 f q -- here, when we call f we will need to
--                             use the binding for q from the outer
--                             scope, which is the same as the calling
--                             scope because we don't have first class
--                             functions. we expect the final q to
--                             evaluate to 100
--
funcScopedTable :: SymbolTableMapList -> SymbolTable -> SymbolTable -> 
                   SymbolTable
funcScopedTable locals defunScopeST callScopeST =
  let callerBindings  = allBindings callScopeST
      closureBindings = allBindings defunScopeST
      writeBarrierBindings =
        ( Map.fromList .
          map (\k -> (k, BoundValue 0.0)) .
          filter (\k -> k `Map.notMember` closureBindings) ) $
            Map.keys callerBindings
  in SymbolTable (Map.union (Map.fromList locals) writeBarrierBindings)
                   (Just callScopeST)
  where allBindings (SymbolTable map Nothing) = map
        allBindings (SymbolTable map (Just p)) = (Map.union map (allBindings p))


retrBinding :: Char -> SymbolTable -> Binding
retrBinding sym (SymbolTable map parent) =
  case Map.lookup sym map of
    Just binding -> binding
    Nothing -> case parent of Just symTab -> retrBinding sym symTab
                              Nothing -> BoundValue 0.0

setBinding :: SymbolTable -> Char -> Binding -> SymbolTable
setBinding symTab sym binding =
  case lookUpward (Just symTab) of
    (False, _) -> let SymbolTable map mParent = symTab
                  in update map mParent
    (True, Just symTab') -> symTab'
  where lookUpward Nothing = (False, Nothing)
        lookUpward (Just (SymbolTable map parent)) =
          case Map.lookup sym map of
             Just binding -> (True, Just $ update map parent)
             Nothing -> case lookUpward parent of
                          (False, Nothing) -> (False, Nothing)
                          (True, parent') -> (True,
                                              Just $ SymbolTable map parent')
        update map mParent = 
          SymbolTable (Map.insert sym binding map) mParent


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
