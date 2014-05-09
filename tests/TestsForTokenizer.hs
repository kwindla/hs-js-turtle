
module TestsForTokenizer
  where

import Tokenizer

tokenizerTests =
  [ ("single char", "a", [TokenSymbol 'a'])

  , ("three chars", "abc", [ TokenSymbol 'a', TokenSymbol 'b', TokenSymbol 'c'])
    
  , ("char number", "a1", [ TokenSymbol 'a' , TokenNumber 1.0])
  
  , ("number char", "1a", [ TokenNumber 1.0, TokenSymbol 'a'])
    
  , ("multi-digit number", "12347", [ TokenNumber 12347.0 ])
  
  , ("number number", "123 47", [ TokenNumber 123.0, TokenNumber 47.0 ])
  
  , ("strip space", "     123 47  ", [ TokenNumber 123.0, TokenNumber 47.0 ])
      
  , ("assignment", "a=a+1", [ TokenSymbol 'a', TokenEquals, TokenSymbol 'a'
                            , TokenOperator Plus, TokenNumber 1.0 ])

  , ("parens", "a=(a+1)", [ TokenSymbol 'a', TokenEquals, TokenLeftParen
                          , TokenSymbol 'a', TokenOperator Plus
                          , TokenNumber 1.0, TokenRightParen ])

  , ("braces", "{a=(a+1)}", [ TokenLeftBrace, TokenSymbol 'a', TokenEquals
                            , TokenLeftParen, TokenSymbol 'a'
                            , TokenOperator Plus, TokenNumber 1.0
                            , TokenRightParen, TokenRightBrace ])
      
  , ("defun", "&", [ TokenDefun ])
  
  , ("if", "?", [ TokenIf ])
    
  , ("repeat", "#", [ TokenRepeat ])
    
  , ("plus", "+", [ TokenOperator Plus ])
  , ("minus", "-", [ TokenOperator Minus ])
  , ("times", "*", [ TokenOperator Times ])
  , ("div", "/",  [ TokenOperator Div ])
  , ("greater than", ">", [ TokenOperator GreaterThan ])
  , ("less than", "<", [ TokenOperator LessThan ])
  , ("equals", "~", [ TokenOperator Equals ])
  ]

