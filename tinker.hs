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

data Turtle = Turtle { heading :: Double
                     , pos     :: (Double, Double)
                     , color   :: (Int, Int, Int)
                     } deriving (Show)
-- type TSL = ( Turtle, SymbolTable, [String] )
type EvalContext = State TSL Double

-- newEvalContext = TSL (Turtle 0 (0,0) (0,0,0)) globalTable  []

getST = getST''

getSTReg :: State TSL SymbolTable
getSTReg = do
  tsl <- get
  return $ _st tsl
  
get' :: (TSL -> a) -> State TSL a
get' f = do
  tsl <- get
  return $ f tsl

getST' :: State TSL SymbolTable
getST' = state (\s -> (s,s)) >>=
           (\tsl -> return $ _st tsl)

getST'' :: State TSL SymbolTable
getST'' = state (\s -> (s,s)) >>=
           (\tsl ->
             state (\s' -> (_st tsl, s')))
           
getST''' :: State TSL SymbolTable
getST''' = get >>= (\tsl -> return $ _st tsl)

           


  
-- return :: a -> State s a
-- return x = state ( \st -> (x, st) )

-- getx :: (TSL -> a) -> State TSL a
-- getx f = get >>= (\s -> return $ f s)
-- getx f = get >>= (\s -> f s) >>= 

--return (f s) = state ( \st -> (f s, st))
-- getx f = get >>= (\s -> return $ f s)


get'' :: (TSL -> a) -> (a -> b) -> State TSL b
get'' f1 f2 = do
  tsl <- get
  let x = f1 tsl
  return $ f2 x

putST :: SymbolTable -> State TSL ()
putST st = do
  tsl <- get
  put $ tsl { _st = st }

getBinding :: Char -> State TSL Binding
getBinding sym = do
  st <- getST
  return $ retrBinding sym st

appendOutput :: String -> State TSL Double
appendOutput str = do
  tsl <- get
  put $ tsl { _ls = (_ls tsl) ++ [str] }
  return 0

-- getTurtle :: State TSL Turtle
-- getTurtle = do
--   (turtle, _, _) <- get
--   return turtle

-- putTurtle :: Turtle -> State TSL ()
-- putTurtle turtle = do
--   (_, st, lines) <- get
--   put $ (turtle, st, lines)


-- getHeading :: State TSL Double
-- getHeading = do
--   turtle <- getTurtle
--   return $ heading turtle

-- putHeading :: Double -> State TSL Double
-- putHeading num = do
--   turtle <- getTurtle
--   putTurtle turtle { heading = num }
--   return num

data TSL = TSL { _turtle :: Turtle, _st :: SymbolTable, _ls :: [String] }
  deriving (Show)
newTSL = TSL (Turtle 90.0 (1.0,1.0) (2,3,4)) globalTable [] 

evaluate :: ExprTree -> EvalContext

evaluate (Assignment sym exprt) = do
  num <- evaluate exprt
  updateBinding sym (BoundValue num)
     
evaluate (Symbol sym) = do
  binding <- getBinding sym
  case (binding) of
      (BoundValue num) -> return num
      otherwise -> error "shouldn't see other bindings here in Symbol eval def"

evaluate (Funcall arity sym exprl) = do
  binding <- getBinding sym
  case (binding) of
    (BoundBuiltin arityInTable f) ->
      if (arityInTable /= arity)
        then error $ "mismatch in arg count for " ++ [sym]
        else f exprl

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
     
runString :: String -> [Double]
runString str = evalState
                  (mapM evaluate ((parse . tokenize) str)) newTSL

pgmString :: String -> [String]
pgmString str = let tsl = execState (
                      mapM evaluate ((parse . tokenize) str)) newTSL
                in _ls tsl

--
 
data Binding = BoundValue   Double                      |
               BoundBuiltin Int ([ExprTree] -> EvalContext)  | 
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
   ,('F', BoundBuiltin 1 (\exprts -> do
                             n <- evaluate $ head exprts
                             appendOutput ("FORWARD " ++ (show n))
                             return n))
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
  st <- getST
  putST $ case st of
    (ScopedTable map parent) ->
      ScopedTable (Map.insert sym binding map) parent
    (GlobalTable map) ->
      GlobalTable $ Map.insert sym binding map
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

