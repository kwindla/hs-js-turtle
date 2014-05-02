module Parser
       where

import Tokenizer
import ExprTree
import SymbolTable
import qualified Data.Map as Map
import Control.Monad.State.Strict


-- Expression-List  -> Expression Expression-List |
--                     empty
-- Expression       -> Symbol = Expression        |
--                     & Symbol Number Expression |
--                     Term Expression-Tail Comparison
-- Expression-Tail  -> + Term Expression-Tail |
--                     - Term Expression-Tail |
--                     empty
-- Comparison       -> < Expression | > Expression | empty 
-- Term             -> Factor Term-Tail
-- Term-Tail        -> * Factor Term-Tail |
--                     / Factor Term-Tail |
--                     empty
-- Factor           -> ( Expression )                     |
--                     { Factor-Expression-List           |
--                     ? Expression Expression Expression |
--                     # Expression Expression            |
--                     [+-] Factor                        | 
--                     Number                             |
--                     Symbol
-- Factor-Expression-List -> } |
--                           Expression Factor-Expression-List
--   


parse :: SymbolTable -> [Token] -> ExprList
parse symtab tokens = evalState expressionList (tokens, symtab)

parseDbg :: String -> String
parseDbg = show . parse (SymbolTable (Map.fromList []) Nothing) . tokenize
--
  
type ParseState = ([Token], SymbolTable)

pcSetTokList tokens     = modify $ \s -> (tokens, snd s)
pcSetSymTab  st         = modify $ \s -> (fst s , st)
-- FIX: clean this and the other updateBinding call up. they should
-- share code. and shouldn't have to have the ugly case statement in
-- the middle of the logic
pcUpdateBinding sym binding = do
  (_, (SymbolTable map mParent)) <- get
  pcSetSymTab $ SymbolTable (Map.insert sym binding map) mParent
  return 0

--

expressionList :: State ParseState ExprList
expressionList = do
  (tokens, _) <- get
  case tokens of
    [] -> return []
    otherwise -> liftM2 (:) expression expressionList

expression :: State ParseState ExprTree
expression = do
  (tokens, _) <- get
  case tokens of
    ((TokenSymbol sym):TokenEquals:ts) -> do                    -- assignment
      pcSetTokList ts
      (Assignment sym) `liftM` expression
    (TokenDefun:(TokenSymbol sym):(TokenNumber arity):ts) -> do -- defun
      -- we need to store the arity of the function defun so we can
      -- parse correctly. we don't care about anything other than the
      -- arity, so we'll just add a placeholder binding with a
      -- do-nothing lambda in the function slot
      pcSetTokList ts
      pcUpdateBinding sym $ BoundDefun (truncate arity) (ExprTreeListNode []) (SymbolTable (Map.fromList []) Nothing)
      Defun sym (truncate arity) `liftM` expression
    otherwise -> term >>= expressionTail >>= comparison
                          
term :: State ParseState ExprTree
term = factor >>= termTail

expressionTail :: ExprTree -> State ParseState ExprTree
expressionTail exprt = do
  (tokens, _) <- get
  case tokens of
    (TokenOperator Plus:ts) -> do
      pcSetTokList ts
      exprt' <- term
      expressionTail $ BinaryOp (BinaryFunc "+" (+)) exprt exprt'
    (TokenOperator Minus:ts) -> do
      pcSetTokList ts
      exprt' <- term
      expressionTail $ BinaryOp (BinaryFunc "-" (-)) exprt exprt'
    otherwise -> return exprt

comparison :: ExprTree -> State ParseState ExprTree
comparison exprt = do
  (tokens, _) <- get
  case tokens of
    (TokenOperator GreaterThan:ts) -> do
      pcSetTokList ts
      exprt' <- expression
      return $ BinaryOp (BinaryFunc ">" gtFunc) exprt exprt'
    (TokenOperator LessThan:ts) -> do
      pcSetTokList ts
      exprt' <- expression
      return $ BinaryOp (BinaryFunc "<" ltFunc) exprt exprt'
    otherwise -> return exprt
  where gtFunc l r = if (l > r) then 1.0 else 0.0
        ltFunc l r = if (l < r) then 1.0 else 0.0


factor :: State ParseState ExprTree
factor = do
  (token:ts, st) <- get
  pcSetTokList ts
  case token of
     TokenNumber num -> return $ ConstantNumber num
     TokenSymbol c ->
       case retrBinding c st of
         BoundValue _ -> return $ Symbol c
         BoundBuiltin arity _ -> do
           exprl <- replicateM arity expression
           return $ Funcall arity c exprl
         BoundDefun arity _ _  -> do
           exprl <- replicateM arity expression
           return $ Funcall arity c exprl           
     TokenLeftParen -> do
       exprt <- expression
       (token':ts', _) <- get
       if token' == TokenRightParen
          then do { pcSetTokList ts' ; return $ exprt }
          else error "saw something other than close paren"
     TokenLeftBrace -> do
       pcSetSymTab $ derivedTable [] st
       exprl <- factorExpressionList
       pcSetSymTab st
       return $ ExprTreeListNode exprl
     TokenIf -> liftM3 TernaryIf expression expression expression
     TokenRepeat -> liftM2 Repeat expression expression
     TokenOperator Plus -> expression
     TokenOperator Minus -> UnaryOp (UnaryFunc "-" negate) `liftM` expression

factorExpressionList :: State ParseState ExprList
factorExpressionList = do
  (tokens, _) <- get
  case tokens of
    [] -> error "unexpected end of token stream inside exprlist"
    ((TokenRightBrace):ts) -> pcSetTokList ts >> return []
    otherwise -> liftM2 (:) expression factorExpressionList

termTail :: ExprTree -> State ParseState ExprTree
termTail exprt = do
  (tokens, _) <- get
  case tokens of
    (TokenOperator Times:ts) -> do
      pcSetTokList ts
      exprt' <- term
      termTail $ BinaryOp (BinaryFunc "*" (*)) exprt exprt'
    (TokenOperator Div:ts) -> do
      pcSetTokList ts
      exprt' <- term
      termTail $ (BinaryOp (BinaryFunc "/" (/)) exprt exprt')
    otherwise -> return exprt




