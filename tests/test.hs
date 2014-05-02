
import Test.Tasty
import Test.Tasty.HUnit

import TurtlePrimitives
import Tokenizer
import Parser
import SymbolTable
import ExprTree

main = defaultMain t_haskell

-- top level test group --

t_haskell :: TestTree
t_haskell = testGroup "haskell" [t_tokenizer, t_parser, t_evaluator]


-- tokenizer tests --

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


-- parser tests --

th_parse = (parse globalTable . tokenize) -- test-helper parse

t_parser_basics :: TestTree
t_parser_basics = testGroup "basics"
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
t_parser_precedence = testGroup "precedence"
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
-- FIX: add stateful arg cound checking on defuns


-- evaluator tests --

th_evals = fst . runString  -- test-helper evaluate for values

t_evaluator :: TestTree
t_evaluator = testGroup "evaluator" [t_eval_vals, t_eval_scope]

t_eval_vals :: TestTree
t_eval_vals = testGroup "simple values"
  [ testCase "single char" $ th_evals "a" @?= [0]
  , testCase "single number" $ th_evals "12347" @?= [12347]
  , testCase "three simple values" $ th_evals "a 1 2" @?= [0,1,2]
  , testCase "assignment" $ th_evals "a=23 a" @?= [23, 23]
  , testCase "defun" $ th_evals "&f2{a}" @?= [2]
  , testCase "if (t)" $ th_evals "?1 100 200" @?= [100]
  , testCase "if (f)" $ th_evals "?0 100 200" @?= [200]
  , testCase "if (t from var)" $ th_evals "a=1 ?a 100 200" @?= [1,100]
  , testCase "if (f from var)" $ th_evals "a=0 ?a 100 200" @?= [0,200]
  , testCase "repeat" $ th_evals "a=1 #4{a=a+1}" @?= [1,5]
  , testCase "unary +" $ th_evals "a=1 (+a)" @?= [1,1]
  , testCase "unary -" $ th_evals "a=1 (-a)" @?= [1,-1]
  , testCase "funcall" $ th_evals "a=1 &f0a f" @?= [1,0,1]
  , testCase "{list}" $ th_evals "{1 2 3 4}" @?= [4]
  ]
  
t_eval_scope :: TestTree
t_eval_scope = testGroup "scope"
  [ testCase "basic block" $ th_evals "{a=23} a" @?= [23, 0]
  , testCase "lexical inherit" $ th_evals "a=0 {a=23} a" @?= [0, 23, 23]
  , testCase "defun" $ th_evals "&f0{a} a=23 f" @?= [0, 23, 0]

  , testCase "lexical x 2" $ th_evals "a=10 {a=12 {a=23} 17} a" @?= [10, 17, 23]
    
  , testCase "closure r" $
      th_evals "a=12 &f0{a} a=23 f a" @?= [12, 0, 23, 23, 23]
  , testCase "closure w-1" $
      th_evals "a=12 &f0{a=a+1} a=23 f a" @?= [12, 0, 23, 24, 24]
  , testCase "closure w-2" $
      th_evals "a=12 &f0{a=a+1} f a" @?= [12, 0, 13, 13]
  , testCase "..." $
      th_evals "&f0{a=a+1} a=23 f a" @?= [0, 23, 1, 23]
      
  , testCase "# funcall" $
      th_evals "&f0{a=a+1} #10f a" @?= [0, 1, 0]
  , testCase "... closure" $
      th_evals "a=1 &f0{a=a+1} #10f a" @?= [1, 0, 11, 11]

  , testCase "# nested" $
      th_evals "#10#10a=a+1 a" @?= [100, 100]
  , testCase "# nested block w/ local" $
      th_evals "#10{#10{a=a+1}} a" @?= [10, 0]
  , testCase "# nested block w/ lexical inherit" $
      th_evals "a=100 #10{#10{a=a+1}} a" @?= [100, 200, 200]

  , testCase "funcall 1 arg" $
      th_evals "&f1{a} f23" @?= [1, 23]
  , testCase "... closure" $
      th_evals "a=100 &f1{a} f23" @?= [100, 1, 23]
  , testCase "funcall 2 args" $
      th_evals "&f2{a+b} f23 24" @?= [2, 47]
  , testCase "... closure" $
      th_evals "a=100 b=10 &f2{a+b} f23 24 a b" @?= [100, 10, 2, 47, 100, 10]
  , testCase "... w" $
      th_evals "a=100 b=10 &f2{a+b} a=12 b=13 f23 24 a b" @?=
      [100, 10, 2, 12, 13, 47, 12, 13]  
  , testCase "three args + closure" $
      th_evals "d=100 &f3{a+b+c+d} f1 2 3 a b c d" @?=
      [100, 3, 106, 0, 0, 0, 100]  
  
  ]
  
-- FIX: quickcheck fuzz for math, checked against haskell evaluation
