module Tokenizer
       where

import Data.Char

data Operator = Plus | Minus | Times | Div | GreaterThan | LessThan 
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
  | c == '+'          = TokenOperator Plus  : tokenize cs
  | c == '-'          = TokenOperator Minus : tokenize cs
  | c == '*'          = TokenOperator Times : tokenize cs
  | c == '/'          = TokenOperator Div   : tokenize cs
  | c == '>'          = TokenOperator GreaterThan : tokenize cs
  | c == '<'          = TokenOperator LessThan    : tokenize cs
  | c == '&'          = TokenDefun            : tokenize cs
  | c == '?'          = TokenIf               : tokenize cs
  | c == '#'          = TokenRepeat           : tokenize cs
  | isDigit c         = let (numstr, cs') = span isDigit (c:cs)
                        in TokenNumber (read numstr) : tokenize cs'
  | otherwise         = error $ "could not tokenize " ++ [c]

