--


import Data.Char


--

main = print $ parse $ tokenize "2 + (4 + 3 + (7 - 5) * 300)" 

--


-- tokenize simple operators and integers from a string
--   e.g. "2 + (3 * 5) - 7
--

data Operator = Plus | Minus | Times | Div 
  deriving (Show, Eq)

data Token = TokenOperator Operator |
             TokenNumber   Double   |
             TokenLeftParen         |
             TokenRightParen        |
             TokenLeftBrace         |
             TokenRightBrace
  deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | isSpace c         = tokenize cs
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
-- Expression       -> Term Expression-Tail
-- Expression-Tail  -> + Term Expression-Tail |
--                     - Term Expression-Tail |
--                     empty
-- Term             -> Factor Term-Tail
-- Term-Tail        -> * Factor Term-Tail |
--                     / Factor Term-Tail |
--                     empty
-- Factor           -> ( Expression )           |
--                     { Factor-Expression-List |
--                     [+-] Expression          | 
--                     Number
-- Factor-Expression-List -> } |
--                           Expression Factor-Expression-List
--   
-- 

parse :: [Token] -> Double
parse tokens = let (emptyTokenSequence, result) = expressionList tokens 0
               in result

-- all of the grammar production functions take a list of 
--   tokens and an accumulatar of the calc result
--   ... and they all return a tuple of same

expressionList :: [Token] -> Double -> ([Token], Double)
expressionList [] result = ([], result)
expressionList tokens result =
    let (tokens', result') = expression tokens result
    in                       expressionList tokens' result'

expression :: [Token] -> Double -> ([Token], Double)
expression tokens result =
    let (tokens', result') = term tokens result
    in                       expressionTail tokens' result'

term :: [Token] -> Double -> ([Token], Double)
term tokens result =
    let (tokens', result') = factor tokens result
    in                       termTail tokens' result'

expressionTail :: [Token] -> Double -> ([Token], Double)
expressionTail [] result = ([], result)
expressionTail (t:ts) result
  | t == (TokenOperator Plus) = 
      let (tokens', result') = term ts result
      in                       expressionTail tokens' (result + result')
  | t == (TokenOperator Minus) =
      let (tokens', result') = term ts result
      in                       expressionTail tokens' (result - result')
  | otherwise = (t:ts, result)

factor :: [Token] -> Double -> ([Token], Double)
factor ((TokenNumber num):ts) result = (ts, num)
factor ((TokenLeftParen):ts) result = 
    let (tokens', result') = expression ts result
    in if head tokens' == TokenRightParen
       then (tail tokens', result')
       else error "saw something other than close paren"
factor ((TokenLeftBrace):ts) result = 
    let (tokens', result') = factorExpressionList ts result
    in                       (tokens', result')
factor ((TokenOperator Plus):ts) result = expression ts result
factor ((TokenOperator Minus):ts) result = 
    let (tokens', result') = expression ts result
    in                       (tokens', -1 * result')

factorExpressionList :: [Token] -> Double -> ([Token], Double)
factorExpressionList [] result = 
    error "unexpected end of token stream inside exprlist"
factorExpressionList ((TokenRightBrace):ts) result = (ts, result)
factorExpressionList tokens result =
    let (tokens', result') = expression tokens result
    in                       factorExpressionList tokens' result'

termTail :: [Token] -> Double -> ([Token], Double)
termTail (t:ts) result
  | t == (TokenOperator Times) =
      let (tokens', result') = factor ts result
      in                       termTail tokens' (result * result')
  | t == (TokenOperator Div) =
      let (tokens', result') = term ts result
      in                       termTail tokens' (result / result')
  | otherwise = (t:ts, result)
termTail [] result = ([], result)
