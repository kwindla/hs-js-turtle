{-# LANGUAGE DeriveDataTypeable #-}

module Tokenizer
       where

import Data.Typeable  -- used by our custom Test.Hasty node testrunner
import Data.Char

data Operator = Plus | Minus | Times | Div | GreaterThan | LessThan |
                Equals | Not | Mod
  deriving (Show, Read, Eq, Typeable)

data Token = TokenEquals            |
             TokenOperator Operator |
             TokenNumber   Double   |
             TokenSymbol   Char     |
             TokenComma             |
             TokenLeftParen         |
             TokenRightParen        |
             TokenLeftBrace         |
             TokenRightBrace        |
             TokenDefun             |
             TokenIf                |
             TokenRepeat
           deriving (Show, Read, Eq, Typeable)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isSpace c         = tokenize cs
  | isAlpha c         = TokenSymbol c : tokenize cs
  | c == ','          = TokenComma : tokenize cs
  | c == '='          = TokenEquals     : tokenize cs
  | c == '('          = TokenLeftParen  : tokenize cs
  | c == ')'          = TokenRightParen : tokenize cs
  | c == '{'          = TokenLeftBrace  : tokenize cs
  | c == '}'          = TokenRightBrace : tokenize cs
  | c == '~'          = TokenOperator Equals : tokenize cs
  | c == '+'          = TokenOperator Plus  : tokenize cs
  | c == '-'          = TokenOperator Minus : tokenize cs
  | c == '*'          = TokenOperator Times : tokenize cs
  | c == '/'          = TokenOperator Div   : tokenize cs
  | c == '\\'         = TokenOperator Mod   : tokenize cs
  | c == '!'          = TokenOperator Not   : tokenize cs
  | c == '>'          = TokenOperator GreaterThan : tokenize cs
  | c == '<'          = TokenOperator LessThan    : tokenize cs
  | c == '&'          = TokenDefun            : tokenize cs
  | c == '?'          = TokenIf               : tokenize cs
  | c == '#'          = TokenRepeat           : tokenize cs
  | isDigit c         = let (numstr, cs') = whileNum (c:cs) ""
                        in TokenNumber (read numstr) : tokenize cs'
  | otherwise         = error $ "could not tokenize " ++ [c]
  where whileNum (c : cs) acc
          | isDigit c                         = whileNum cs (acc ++ [c])
          | c == '.' && (not $ elem '.' acc)  = whileNum cs (acc ++ [c])
        whileNum cs acc = (acc, cs)


