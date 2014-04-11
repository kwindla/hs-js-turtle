--


import Data.Char
import qualified Data.Map as Map
import Control.Monad.State


--

-- do something like - runString "P{A=4 P=2 P+A}PA"
--                or - (parse . tokenize) "a=4 P"

--

data Operator = Plus | Minus | Times | Div 
  deriving (Show, Eq)

data Token = TokenEquals            |
             TokenOperator Operator |
             TokenNumber   Double   |
             TokenSymbol   Char     |
             TokenLeftParen         |
             TokenRightParen        |
             TokenLeftBrace         |
             TokenRightBrace        |
             TokenDefun             |
             TokenIf                |
             TokenRepeat
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isSpace c         = tokenize cs
  | isAlpha c         = TokenSymbol c : tokenize cs
  | c == '='          = TokenEquals     : tokenize cs
  | c == '('          = TokenLeftParen  : tokenize cs
  | c == ')'          = TokenRightParen : tokenize cs
  | c == '{'          = TokenLeftBrace  : tokenize cs
  | c == '}'          = TokenRightBrace : tokenize cs
  | c == '+'          = TokenOperator Plus    : tokenize cs
  | c == '-'          = TokenOperator Minus   : tokenize cs
  | c == '*'          = TokenOperator Times   : tokenize cs
  | c == '/'          = TokenOperator Div     : tokenize cs
  | c == '&'          = TokenDefun            : tokenize cs
  | c == '?'          = TokenIf               : tokenize cs
  | c == '#'          = TokenRepeat           : tokenize cs
  | isDigit c         = let (numstr, cs') = span isDigit (c:cs)
                        in TokenNumber (read numstr) : tokenize cs'
  | otherwise         = error $ "could not tokenize " ++ [c]


-- and a grammar for evaluating resulting list of tokens
--   this started as a standard LL-parseable calculator 
--   grammar cribbed from the interwebs, then got crufty

-- Expression-List  -> Expression Expression-List |
--                     empty
-- Expression       -> Symbol = Expression        |
--                     & Symbol Number Expression |
--                     Term Expression-Tail
-- Expression-Tail  -> + Term Expression-Tail |
--                     - Term Expression-Tail |
--                     empty
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

data UnaryFunc = UnaryFunc String (Double -> Double)
instance Show UnaryFunc where
  show (UnaryFunc name f) = "(" ++ name ++ ")"
    
data BinaryFunc = BinaryFunc String (Double -> Double -> Double)
instance Show BinaryFunc where
  show (BinaryFunc name f) = "(" ++ name ++ ")"

type ExprList = [ExprTree]
data ExprTree = Assignment Char ExprTree              |
                Defun Char Int ExprTree               |
                TernaryIf ExprTree ExprTree ExprTree  |
                Repeat    ExprTree ExprTree           |
                UnaryOp UnaryFunc ExprTree            |
                BinaryOp BinaryFunc ExprTree ExprTree |
                ConstantNumber Double                 |
                Symbol Char                           |
                Funcall Int Char ExprList             |
                ExprTreeListNode ExprList
              deriving (Show)


parse :: [Token] -> ExprList
parse tokens = let (emptyTokenSequence, exprs) = expressionList tokens []
               in exprs

-- 

expressionList :: [Token] -> ExprList -> ([Token], ExprList)
expressionList [] exprs = ([], exprs)
expressionList tokens exprs =
  let (tokens', tree) = expression tokens
  in                    expressionList tokens' (exprs ++ [tree])

expression :: [Token] -> ([Token], ExprTree)
expression ((TokenSymbol sym):TokenEquals:ts) =                   -- assignment
  let (tokens, exprt) = expression ts
  in                    (tokens, Assignment sym exprt)
expression (TokenDefun:(TokenSymbol sym):(TokenNumber num):ts) =  -- defun
  let (tokens, exprt) = expression ts
  in                    (tokens, Defun sym (truncate num) exprt)
expression tokens =
  let (tokens', exprt') = term tokens 
  in                      expressionTail tokens' exprt'
term :: [Token] -> ([Token], ExprTree)
term tokens =
  let (tokens', exprt) = factor tokens
  in                     termTail tokens' exprt

expressionTail :: [Token] -> ExprTree -> ([Token], ExprTree)
expressionTail [] exprt = ([], exprt)
expressionTail (t:ts) exprt
  | t == (TokenOperator Plus) =
    let (tokens', exprt') = term ts
    in expressionTail tokens' (BinaryOp (BinaryFunc "+" (+)) exprt exprt')
  | t == (TokenOperator Minus) =
    let (tokens', exprt') = term ts
    in expressionTail tokens' (BinaryOp (BinaryFunc "-" (-)) exprt exprt')
  | otherwise = (t:ts, exprt)

factor :: [Token] -> ([Token], ExprTree)
factor ((TokenNumber num):ts) = (ts, ConstantNumber num)
factor ((TokenSymbol c):ts) = 
  case retrBinding c globalTable of
    BoundValue _ -> (ts, Symbol c)
    BoundBuiltin arity _ -> parseFurther arity arity c ts []
    BoundDefun arity _   -> parseFurther arity arity c ts []
  where parseFurther 0 arity c tokens exprl = (tokens, Funcall arity c exprl)
        parseFurther n arity c tokens exprl =
          let (tokens', exprt) = expression tokens 
          in parseFurther (n-1) arity c tokens' (exprl ++ [exprt])
        
factor ((TokenLeftParen):ts) = 
  let (tokens', exprt) = expression ts
  in if head tokens' == TokenRightParen
     then (tail tokens', exprt)
     else error "saw something other than close paren"
factor ((TokenLeftBrace):ts) = 
  let (tokens', exprl) = factorExpressionList ts []
  in                     (tokens', ExprTreeListNode exprl)
factor (TokenIf:ts) =
  let (ts', exprt) = expression ts
  in let (ts'', exprt') = expression ts'
     in let (ts''', exprt'') = expression ts''
        in (ts''', TernaryIf exprt exprt' exprt'')
factor (TokenRepeat:ts) =
  let (ts', exprt) = expression ts
  in let (ts'', exprt') = expression ts'
     in (ts'', Repeat exprt exprt')

factor ((TokenOperator Plus):ts) = expression ts
factor ((TokenOperator Minus):ts) =        -- (-1 * result')
  let (tokens', exprt) = factor ts
  in (tokens', UnaryOp (UnaryFunc "-" negate) exprt)

factorExpressionList :: [Token] -> ExprList -> ([Token], ExprList)
factorExpressionList [] _ = 
  error "unexpected end of token stream inside exprlist"
factorExpressionList ((TokenRightBrace):ts) exprl = (ts, exprl)
factorExpressionList tokens exprl =
  let (tokens', exprl') = expression tokens
  in                      factorExpressionList tokens' (exprl ++ [exprl'])

termTail :: [Token] -> ExprTree -> ([Token], ExprTree)
termTail (t:ts) exprt
  | t == (TokenOperator Times) =
    let (tokens', exprt') = factor ts
    in termTail tokens' (BinaryOp (BinaryFunc "*" (*)) exprt exprt')
  | t == (TokenOperator Div) =
    let (tokens', exprt') = term ts
    in termTail tokens' (BinaryOp (BinaryFunc "/" (/)) exprt exprt')
  | otherwise = (t:ts, exprt)
termTail [] exprt = ([], exprt)

--
-- recursive evaluation
--
--

-- State Monad to implicitly thread through the evaluator recursion
--
type EvalContext = State SymbolTable Double


evaluate :: ExprTree -> EvalContext

evaluate (Assignment sym exprt) = do
  num <- evaluate exprt
  updateBinding sym (BoundValue num)
     
evaluate (Symbol sym) = do
  st <- get
  case (retrBinding sym st) of
      (BoundValue num) -> return num
      otherwise -> error "shouldn't see other bindings here in Symbol eval def"

evaluate (Funcall arity sym exprl) = do
  st <- get
  case (retrBinding sym st) of
    (BoundBuiltin arityInTable f) ->
      if (arityInTable /= arity)
        then error $ "mismatch in arg count for " ++ [sym]
        else return (f exprl)

evaluate (UnaryOp (UnaryFunc _ f) exprt) = liftM f (evaluate exprt)
evaluate (BinaryOp (BinaryFunc _ f) left right) =
  liftM2 f (evaluate left) (evaluate right)  
evaluate (ConstantNumber num) = return num

evaluate (TernaryIf eCond eIf eThen) = do
  num <- evaluate eCond
  if (not (num==0))
    then evaluate eIf
    else evaluate eThen

evaluate (ExprTreeListNode exprl) = do
  original_st <- get
  num <- evaluateExprList exprl 0
  put original_st   -- put back the original symbol table, not the list
  return num        -- block's modded table. we're "popping the stack"
                    -- when exiting the expression list block

-- two definitions of evaluate on Repeat. the first one is specialized
-- for expression lists in the repeatee position. only difference is
-- that we hand-manage the symbol table context in the block case,
-- treating the loop as one context, then "popping" the modified
-- symbol table when the repeat finishes.
-- evaluate (Repeat eNumTimes (ExprTreeListNode exprl)) =
--   let (st', numTimes) = evaluate st eNumTimes
--       (_, lastResult) = repeat numTimes st' exprl 0
--   in (st', lastResult)
--   where repeat 0 st'' _ last  = (st'', last)
--         repeat n st'' exprl _ =
--           let (st''', result) = evaluateExprList st'' exprl 0
--           in repeat (n-1) st''' exprl result
-- evaluate (Repeat eNumTimes exprt) =
--   let (st', numTimes) = evaluate st eNumTimes
--   in repeat numTimes st' exprt 0
--   where repeat 0 st'' _ last  = (st'', last)
--         repeat n st'' exprt _ =
--           let (st''', result) = evaluate st'' exprt
--           in repeat (n-1) st''' exprt result

evaluateExprList :: ExprList -> Double -> EvalContext
evaluateExprList [] lastResult = return lastResult
evaluateExprList (e:es) _ = do
  result <- evaluate e
  evaluateExprList es result

--

runString :: String -> [Double]
runString str =
  let exprl           = (parse . tokenize) str
      (symt, numbers) = _eval globalTable exprl []
  in numbers
    where _eval symt [] accum = (symt, accum)
          _eval symt (e:es) accum =
            let (num, symt') = runState (evaluate e) symt
            in _eval symt' es (accum ++ [num])

  -- map (\(st,num) -> num) $
  --              map (evaluate globalTable) $ (parse . tokenize) str

--
--
 
data Binding = BoundValue   Double                      |
               BoundBuiltin Int ([ExprTree] -> Double)  | 
               BoundDefun   Int (ExprTree -> [ExprTree] -> Double)
instance Show Binding where
  show (BoundValue num) = "`" ++ (show num)

type SymbolTableMapList = [(Char, Binding)]
type SymbolTableMap = (Map.Map Char Binding)

data SymbolTable = ScopedTable SymbolTableMap SymbolTable |
                   GlobalTable SymbolTableMap
  deriving (Show)

globalTable = GlobalTable $ Map.fromList 
  [ ('P', BoundValue pi)
   ,('F', BoundBuiltin 1 (\exprts -> 456.3))
  ]

derivedTable :: SymbolTableMapList -> SymbolTable -> SymbolTable
derivedTable list parent = ScopedTable (Map.fromList list) parent

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
  st <- get
  put $ case st of
    (ScopedTable map parent) -> ScopedTable (Map.insert sym binding map) parent
    (GlobalTable map) -> GlobalTable $ Map.insert sym binding map
  case binding of
    (BoundValue num) -> return num
    otherwise        -> return 0

-- updateBinding :: Char -> Binding -> SymbolTable -> SymbolTable
-- updateBinding sym binding (ScopedTable map parent) =
--   ScopedTable (Map.insert sym binding map) parent
-- updateBinding sym binding (GlobalTable map) =
--   GlobalTable $ Map.insert sym binding map



--

-- reduce boilerplate in binary operator evaluate() cases by lifting
-- haskell binary operators to work on a pair of expression
-- trees. tempting to generalize this further and rework the evaluate
-- function to be built around functorish lines. that would require
-- some refactoring, though.
-- _el2 :: (Double -> Double -> Double) -> SymbolTable ->
--         ExprTree -> ExprTree -> (SymbolTable, Double)
-- _el2 f st left right =
--   let (st', numl) = evaluate st left
--       (st'', numr) = evaluate st' right
--   in (st'', f numl numr)

