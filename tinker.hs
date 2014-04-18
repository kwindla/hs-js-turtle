
import Data.Char
import qualified Data.Map as Map
import Control.Monad.State
import Text.Printf
import Data.List

-- Examples:
--   echo "#36{R10#8{F25L45}}" | runghc tinker.hs | display svg:-
--   echo "#8{R45#6{#90{F1R2}R90}}" | runghc tinker.hs | display svg:-
-- 
-- to debug...  - runString "P{A=4 P=2 P+A}PA"
--           or - (parse . tokenize) "a=4 P"

main = interact ( intercalate " " . pgmString )


--


newTSL = TSL (Turtle (90.0) (100.0,100.0) (0,0,0)) globalTable svgPrelude 

globalTable = SymbolTable (Map.fromList 
  [ ('P', BoundValue pi)
  , ('F', BoundBuiltin 1 (\exprts -> biForward (head exprts)))
  , ('R', BoundBuiltin 1 (\exprts -> biRotate subtract (head exprts)))
  , ('L', BoundBuiltin 1 (\exprts -> biRotate (+) (head exprts)))
  ]) Nothing

type Point = (Double , Double)
x :: Point -> Double
x p = fst p
y :: Point -> Double
y p = snd p

shp :: (String, String) -> Point -> String
shp n p = printf "%s=\"%.0f\" %s=\"%.0f\"" (fst n) (x p) (snd n) (y p)

vplus :: Point -> Point -> Point
vplus p0 p1 = ((x p0) + (x p1) , (y p0) + (y p1))

vtimes :: Point -> Double -> Point
vtimes p d = ((x p) * d , (y p) * d)

-- turns a heading (in degrees) into a unit vector
directionVectUnit :: Double -> Point
directionVectUnit h =
  let theta = 2 * pi * h / 360
      in (cos(theta) , sin(theta))

directionVect :: Double -> Double -> Point
directionVect h dist = (directionVectUnit h) `vtimes` dist

biForward :: ExprTree -> EvalContext
biForward expr = do
  dist <- evaluate expr
  t <- gets $ view turtle
  let p0 = view pos t
      p1 = p0 `vplus` (directionVect (view heading t) dist)
  modify $ set' turtle pos p1
  appendOutput $ "<line " ++ (shp ("x1","y1") p0) ++ " " ++
    (shp ("x2","y2") p1) ++ " style=\"stroke:rgb(0,0,0);stroke-width:2\" />"
  -- appendOutput ("FORWARD " ++ (showf p0) ++ " -> " ++ (showf p1))
  return dist

biRotate :: (Double -> Double -> Double) -> ExprTree -> EvalContext
biRotate f expr = do
  num <- evaluate expr
  modify $ update' turtle heading (f num)
  return num


svgPrelude = ["<svg width=\"200\" height=\"200\">"]
svgPostlude = ["</svg>\n"]


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
parse tokens = evalState expressionList (tokens, globalTable)

-- 

type ParseState = ([Token], SymbolTable)

pcSetTokList tokens     = modify $ \s -> (tokens, snd s)
pcSetSymTab  st         = modify $ \s -> (fst s , st)
-- FIX: clean this and the other updateBinding call up. they should
-- share code. and shouldn't have to have the ugly case statement in
-- the middle of the logic
pcUpdateBinding sym binding = do
  (_, (SymbolTable map parent)) <- get
  pcSetSymTab $ SymbolTable (Map.insert sym binding map) parent
  return 0

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
      pcUpdateBinding sym $ BoundDefun (truncate arity) (ExprTreeListNode []) globalTable
      Defun sym (truncate arity) `liftM` expression
    otherwise -> term >>= expressionTail
                          
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

--


type EvalContext = State TSL Double

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
      putST $ derivedTable bindings fun_st
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
     
runString :: String -> [Double]
runString str = evalState
                  (mapM evaluate ((parse . tokenize) str)) newTSL

pgmString :: String -> [String]
pgmString str = let tsl = execState (
                      mapM evaluate ((parse . tokenize) str)) newTSL
                in (view outLines tsl) ++ svgPostlude


--
 

data Binding = BoundValue   Double                      |
               BoundBuiltin Int ([ExprTree] -> EvalContext)  | 
               BoundDefun   Int ExprTree SymbolTable

instance Show Binding where
  show (BoundValue num) = "`" ++ (show num)
  show (BoundBuiltin arity _) = "#" ++ (show arity)
  show (BoundDefun arity _ _) = "&" ++ (show arity)

type SymbolTableMapList = [(Char, Binding)]
type SymbolTableMap = (Map.Map Char Binding)

-- FIX: -- use a Maybe so that there's only a single ST contructor

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

getST = gets $ view symTab
putST st = modify $ set symTab st  
getBinding sym = gets $ retrBinding sym . view symTab
appendOutput lines = modify $ update outLines (++[lines])    
  
updateBinding sym binding = do
  (SymbolTable map parent) <- getST
  putST $ SymbolTable (Map.insert sym binding map) parent
  case binding of
    BoundValue num -> return num
    otherwise -> return 0.0


derivedTable :: SymbolTableMapList -> SymbolTable -> SymbolTable
derivedTable list parent =
  SymbolTable (Map.fromList list) (Just parent)
  
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
funcScopedTable list scoped dynamic = globalTable
-- HERE

retrBinding :: Char -> SymbolTable -> Binding
retrBinding sym (SymbolTable map parent) =
  let maybeBinding = Map.lookup sym map
  in case maybeBinding of
    Just binding -> binding
    Nothing      -> case parent of
                         Just st@(SymbolTable _ _) -> retrBinding sym st
                         Nothing -> BoundValue 0.0


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
