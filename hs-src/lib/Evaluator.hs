module Evaluator
       where

import SymbolTable
import ExprTree
import Control.Monad.State.Strict

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
    (BoundDefun arityInTable exprt defunScopeST) -> do
      -- we want to evaluate the each item in the args list and bind
      -- the results to a..i in a new, local, symbol table.
      argValues <- mapM evaluate args
      let locals = zip ['a'..'i'] (map BoundValue argValues)
      callScopeST <- getST
      putST $ funcScopedTable locals defunScopeST callScopeST
      result <- evaluate exprt
      (SymbolTable _ (Just originalST')) <- getST
      putST originalST'
      return result
      
evaluate (UnaryOp (UnaryFunc _ f) exprt) = do -- liftM f (evaluate exprt)
  v <- evaluate exprt
  v `seq` return $! f v
evaluate (BinaryOp (BinaryFunc _ f) left right) = do
  -- originally was the very satisfying:
  --   liftM2 f (evaluate left) (evaluate right)  
  -- rewritten to force strict evaluation
  l <- evaluate left
  r <- evaluate right
  l `seq` r `seq` return $! f l r

evaluate (ConstantNumber num) = return num

evaluate (TernaryIf eCond eIf eThen) = do
  num <- evaluate eCond
  if num /= 0 then evaluate eIf else evaluate eThen

evaluate (ExprTreeListNode exprl) = do
  originalST <- getST
  putST $ derivedTable [] originalST
  results <- mapM evaluate exprl
  (SymbolTable _ (Just originalST')) <- getST
  putST originalST'
  return $ last results 

evaluate (Repeat exprNumTimes exprt) = do
  numTimes <- evaluate exprNumTimes
  -- need to handle expression list blocks differently from bare
  -- expressions, here for a block, we want to repeat multiple times
  -- with a stateful symbol table across all repeats, then "pop" that
  -- symbol table and throw it away.
  case exprt of
    (ExprTreeListNode exprl) -> do
      originalST <- getST
      putST $ derivedTable [] originalST
      r <- repeat numTimes (last `liftM` (mapM evaluate exprl)) 0
      (SymbolTable _ (Just originalST')) <- getST
      putST $! originalST'
      return $! r
    otherwise -> do
      r <- repeat numTimes (evaluate exprt) 0
      return $! r
  where repeat n f acc | n <= 0    = return acc
                       | otherwise = do
                           r' <- f
                           repeat (n-1) f r'
