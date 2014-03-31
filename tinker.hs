--


import Data.Char


--

main = print $ evaluate $ head $ parse $ tokenize "2 + (4 + 3 + (7 - 5) * 300)" 

--


-- tokenize simple operators and integers from a string
--   e.g. "2 + (3 * 5) - 7
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
             TokenRightBrace
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
    | isDigit c         = let (numstr, cs') = span isDigit (c:cs)
                          in TokenNumber (read numstr) : tokenize cs'
    | otherwise         = error $ "could not tokenize " ++ [c]

-- and a grammar for evaluating resulting list of tokens
--   this started as a standard LL-parseable calculator 
--   grammar cribbed from the interwebs, then got crufty

-- Expression-List  -> Expression Expression-List |
--                     empty
-- Expression       -> Symbol = Expression        |
--                     Term Expression-Tail
-- Assignment       -> Symbol = Expression
-- Expression-Tail  -> + Term Expression-Tail |
--                     - Term Expression-Tail |
--                     empty
-- Term             -> Factor Term-Tail
-- Term-Tail        -> * Factor Term-Tail |
--                     / Factor Term-Tail |
--                     empty
-- Factor           -> ( Expression )           |
--                     { Factor-Expression-List |
--                     [+-] Factor              | 
--                     Number                   |
--                     Symbol
-- Factor-Expression-List -> } |
--                           Expression Factor-Expression-List
--   

type ExprList = [ExprTree]
data ExprTree = Assignment Char ExprTree     |
                UnaryOpMinus ExprTree        |
                BinOpPlus ExprTree ExprTree  | 
                BinOpMinus ExprTree ExprTree |
                BinOpTimes ExprTree ExprTree |
                BinOpDiv ExprTree ExprTree   |
                ConstantNumber Double        |
                Symbol Char                  |
                ExprTreeListNode ExprList
              deriving (Show)


parse :: [Token] -> ExprList
parse tokens = let (emptyTokenSequence, exprs) = expressionList tokens []
               in exprs

-- 
-- 
-- 

expressionList :: [Token] -> ExprList -> ([Token], ExprList)
expressionList [] exprs = ([], exprs)
expressionList tokens exprs =
    let (tokens', tree) = expression tokens
    in                    expressionList tokens' (exprs ++ [tree])

expression :: [Token] -> ([Token], ExprTree)
expression ((TokenSymbol sym):TokenEquals:ts) = 
    let (tokens, exprt) = expression ts
    in                    (tokens, Assignment sym exprt)
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
  | t == (TokenOperator Plus) =                       -- (result + result')
      let (tokens', exprt') = term ts
      in                      expressionTail tokens' (BinOpPlus exprt exprt')
  | t == (TokenOperator Minus) =                      -- (result - result')
      let (tokens', exprt') = term ts
      in                      expressionTail tokens' (BinOpMinus exprt exprt')
  | otherwise = (t:ts, exprt)

factor :: [Token] -> ([Token], ExprTree)
factor ((TokenNumber num):ts) = (ts, ConstantNumber num)
factor ((TokenSymbol c):ts) = (ts, Symbol c)
factor ((TokenLeftParen):ts) = 
    let (tokens', exprt) = expression ts
    in if head tokens' == TokenRightParen
       then (tail tokens', exprt)
       else error "saw something other than close paren"
factor ((TokenLeftBrace):ts) = 
    let (tokens', exprl) = factorExpressionList ts []
    in                     (tokens', ExprTreeListNode exprl)
factor ((TokenOperator Plus):ts) = expression ts
factor ((TokenOperator Minus):ts) =        -- (-1 * result')
    let (tokens', exprt) = factor ts
    in                     (tokens', UnaryOpMinus exprt)

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
      in                      termTail tokens' (BinOpTimes exprt exprt')
  | t == (TokenOperator Div) =
      let (tokens', exprt') = term ts
      in                      termTail tokens' (BinOpDiv exprt exprt')
  | otherwise = (t:ts, exprt)
termTail [] exprt = ([], exprt)


evaluate :: ExprTree -> Double
evaluate (UnaryOpMinus exprt) = - (evaluate exprt)
evaluate (BinOpPlus left right) = (evaluate left) + (evaluate right)
evaluate (BinOpMinus left right) = (evaluate left) - (evaluate right)
evaluate (BinOpTimes left right) = (evaluate left) * (evaluate right)
evaluate (BinOpDiv left right) = (evaluate left) / (evaluate right)
evaluate (ConstantNumber num) = num
evaluate (ExprTreeListNode exprl) = evalList exprl 0
  where evalList [] lastResult = lastResult
        evalList (e:es) lastResult = evalList es (evaluate e)

--
--
 
