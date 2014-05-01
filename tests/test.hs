
import Test.Tasty
import Test.Tasty.HUnit

import TurtlePrimitives
import Tokenizer
import Parser
import SymbolTable
import ExprTree

main = defaultMain t_haskell

t_haskell :: TestTree
t_haskell = testGroup "haskell" [t_tokenizer, t_parser]

t_tokenizer :: TestTree
t_tokenizer = testGroup "tokenizer" 
  [ testCase "single char" $ tokenize "a" @?= [TokenSymbol 'a']
  
  , testCase "three chars" $ tokenize "abc" @?= [ TokenSymbol 'a'
                                                , TokenSymbol 'b'
                                                , TokenSymbol 'c']
    
  , testCase "char number" $ tokenize "a1" @?= [ TokenSymbol 'a'
                                               , TokenNumber 1.0]
  
  , testCase "number char" $ tokenize "1a" @?= [ TokenNumber 1.0,
                                                 TokenSymbol 'a']
    
  , testCase "multi-digit number" $
      tokenize "12347" @?= [ TokenNumber 12347.0 ]
  
  , testCase "number number" $
      tokenize "123 47" @?= [ TokenNumber 123.0, TokenNumber 47.0 ]
  
  , testCase "strip space" $
      tokenize "     123 47  " @?= [ TokenNumber 123.0, TokenNumber 47.0 ]
      
  , testCase "assignment" $ tokenize "a=a+1" @?=
      [ TokenSymbol 'a', TokenEquals, TokenSymbol 'a', TokenOperator Plus,
        TokenNumber 1.0 ]

  , testCase "parens" $ tokenize "a=(a+1)" @?=
      [ TokenSymbol 'a', TokenEquals, TokenLeftParen,
        TokenSymbol 'a', TokenOperator Plus, TokenNumber 1.0, TokenRightParen ]

  , testCase "braces" $ tokenize "{a=(a+1)}" @?=
      [ TokenLeftBrace, TokenSymbol 'a', TokenEquals, TokenLeftParen,
        TokenSymbol 'a', TokenOperator Plus, TokenNumber 1.0, TokenRightParen, 
        TokenRightBrace ]
      
  , testCase "defun" $ tokenize "&" @?= [ TokenDefun ]
  
  , testCase "if"  $ tokenize "?" @?= [ TokenIf ]
    
  , testCase "repeat"  $ tokenize "#" @?= [ TokenRepeat ]
    
  , testCase "plus"  $ tokenize "+" @?= [ TokenOperator Plus ]
  , testCase "minus"  $ tokenize "-" @?= [ TokenOperator Minus ]
  , testCase "times"  $ tokenize "*" @?= [ TokenOperator Times ]
  , testCase "div"  $ tokenize "/" @?= [ TokenOperator Div ]
  , testCase "greater than"  $ tokenize ">" @?= [ TokenOperator GreaterThan ]
  , testCase "less than"  $ tokenize "<" @?= [ TokenOperator LessThan ]
  ]

t_parser :: TestTree
t_parser = testGroup "parser" [t_parser_basics, t_parser_precedence]

-- test-helper parse
th_parse = (parse globalTable . tokenize)

t_parser_basics :: TestTree
t_parser_basics = testGroup "parser-basics"
  [ testCase "single char" $ th_parse "a" @?= [Symbol 'a']
  
  , testCase "three chars" $ th_parse "abc" @?= [Symbol 'a',
                                                 Symbol 'b',
                                                 Symbol 'c']
    
  , testCase "assignment" $ th_parse "a=a+1" @?=
      [Assignment 'a' (BinaryOp (read "(+)") (Symbol 'a') (ConstantNumber 1.0))]
      
  , testCase "minus" $ th_parse "a-3" @?=
      [BinaryOp (read "(-)") (Symbol 'a') (ConstantNumber 3.0)]
  , testCase "times" $ th_parse "a*3" @?=
      [BinaryOp (read "(*)") (Symbol 'a') (ConstantNumber 3.0)]
  , testCase "div" $ th_parse "a/3" @?=
      [BinaryOp (read "(/)") (Symbol 'a') (ConstantNumber 3.0)]
  , testCase "greater than" $ th_parse "a>3" @?=
      [BinaryOp (read "(>)") (Symbol 'a') (ConstantNumber 3.0)]
  , testCase "less than" $ th_parse "a<3" @?=
      [BinaryOp (read "(<)") (Symbol 'a') (ConstantNumber 3.0)]
      
  , testCase "unary -" $ th_parse "-1" @?=
      [UnaryOp (read "(-)") (ConstantNumber 1.0)]
  , testCase "unary +" $ th_parse "+1" @?= [(ConstantNumber 1.0)]
    
  , testCase "a{bc}" $ th_parse "a{bc}" @?=
      [Symbol 'a', ExprTreeListNode [Symbol 'b',Symbol 'c']]
    
  , testCase "ternary if" $ th_parse "?a>bcd" @?=
      [TernaryIf (BinaryOp (read "(>)") (Symbol 'a') (Symbol 'b'))
         (Symbol 'c') (Symbol 'd')]

  , testCase "repeat" $ th_parse "#a/3x" @?=
      [Repeat (BinaryOp (read "(/)") (Symbol 'a') (ConstantNumber 3.0))
         (Symbol 'x')]
      
  , testCase "defun and funcall" $ th_parse "&S2a Sbc" @?=
      [Defun 'S' 2 (Symbol 'a'), Funcall 2 'S' [Symbol 'b',Symbol 'c']]
  ]
  
t_parser_precedence :: TestTree
t_parser_precedence = testGroup "parser-precedence"
  [ testCase "+*" $ th_parse "a+1*3" @?=
      [BinaryOp (read "(+)") (Symbol 'a')
       (BinaryOp (read "(*)") (ConstantNumber 1.0) (ConstantNumber 3.0))]
    
  , testCase "-/" $ th_parse "a-1/3" @?=
      [BinaryOp (read "(-)") (Symbol 'a')
        (BinaryOp (read "(/)") (ConstantNumber 1.0) (ConstantNumber 3.0))]
      
  , testCase ">" $ th_parse "a+1>4*3" @?=
      [BinaryOp (read "(>)")
        (BinaryOp (read "(+)") (Symbol 'a') (ConstantNumber 1.0))
        (BinaryOp (read "(*)") (ConstantNumber 4.0) (ConstantNumber 3.0))]
      
  , testCase "*+" $ th_parse "a*1+3" @?=
      [BinaryOp (read "(+)")
         (BinaryOp (read "(*)") (Symbol 'a') (ConstantNumber 1.0))
         (ConstantNumber 3.0)]

  , testCase "*(+)" $ th_parse "a*(1+3)" @?=
      [BinaryOp (read "(*)")
          (Symbol 'a')
          (BinaryOp (read "(+)") (ConstantNumber 1.0) (ConstantNumber 3.0))]

  ]

                                    


