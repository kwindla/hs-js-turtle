module Evaluator
       where

import SymbolTable
import ExprTree
import Control.Monad.State


evaluate :: ExprTree -> EvalContext

evaluate (Assignment sym exprt) = do
  num <- evaluate exprt
  updateBinding sym (BoundValue num)
     
evaluate (Symbol sym) = do
  binding <- getBinding sym
  case binding of
    (BoundValue num) -> return num
    otherwise -> error "shouldn't see other bindings here in Symbol eval def"

evaluate (Defun sym arity exprt) = do
  -- we need to put a binding in place (we only put in placeholders
  -- during parse)
  st <- getST
  updateBinding sym $ BoundDefun arity exprt st
  return $ fromIntegral arity

evaluate (Funcall arity sym args) = do
  binding <- getBinding sym
  case (binding) of
    (BoundBuiltin arityInTable f) -> f args
    (BoundDefun arityInTable exprt fun_st) -> do
      -- we want to evaluate the each item in the args list and bind
      -- the results to a..i in a new, local, symbol table.
      argValues <- mapM evaluate args
      let bindings = zip ['a'..'i'] (map BoundValue argValues)
      st <- getST
      putST $ funcScopedTable bindings fun_st st
      result <- evaluate exprt
      putST st
      return result
      
evaluate (UnaryOp (UnaryFunc _ f) exprt) = liftM f (evaluate exprt)
evaluate (BinaryOp (BinaryFunc _ f) left right) =
  liftM2 f (evaluate left) (evaluate right)  
evaluate (ConstantNumber num) = return num

evaluate (TernaryIf eCond eIf eThen) = do
  num <- evaluate eCond
  if num /= 0 then evaluate eIf else evaluate eThen

evaluate (ExprTreeListNode exprl) = do
  original_st <- getST
  results <- mapM evaluate exprl
  putST original_st -- "pop" the stack, putting the original symbol table
                    -- back into our EvalContext state monad
  return $ last results 

evaluate (Repeat exprNumTimes exprt) = do
  numTimes <- evaluate exprNumTimes
  -- need to handle expression list blocks differently from bare
  -- expressions, here for a block, we want to repeat multiple times
  -- with a stateful symbol table across all repeats, then "pop" that
  -- symbol table and throw it away.
  results <- case exprt of
    (ExprTreeListNode exprl) -> do
      original_st <- getST
      r <- replicateM (floor numTimes) (liftM last (mapM evaluate exprl))
      putST original_st
      return r
    otherwise ->
      replicateM (floor numTimes) (evaluate exprt)
  return $ last results