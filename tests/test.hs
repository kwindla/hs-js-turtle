
import TurtlePrimitives
import Tokenizer
import Parser
import SymbolTable
import ExprTree

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestNodeProgram

import TestsForTokenizer
import TestsForParser


main = defaultMain $ testGroup "all" [t_haskell, t_javascript]


-- javascript options (FIX: make these cmd line configurable)
jsExe = "node"
jsSrcDir = "../js-src"


-- top level test group --

t_haskell :: TestTree
t_haskell = testGroup "haskell" [t_tokenizer_hs, t_parser_hs, t_evaluator]

t_javascript :: TestTree
t_javascript = testGroup "javascript" [t_tokenizer_js, t_parser_js]


-- tokenizer tests --

t_tokenizer_hs :: TestTree
t_tokenizer_hs = testGroup "tokenizer" $
  map (\(name, str, expectedTokens) ->
          testCase name $ tokenize str @?= expectedTokens)
    tokenizerTests

t_tokenizer_js ::TestTree
t_tokenizer_js = testGroup "tokenizer" $
    map (\(name, str, expectedTokens) ->
            jsTestScript name jsExe jsSrcDir "Tokenizer" "tokenize"
            str expectedTokens)
      tokenizerTests



-- parser tests --

t_parser_hs :: TestTree
t_parser_hs = testGroup "parser" [t_parser_basics_hs, t_parser_precedence_hs]

t_parser_js :: TestTree
t_parser_js = testGroup "parser" [t_parser_basics_js]


th_parse = (parse globalTable . tokenize) -- test-helper parse
th_parser_test_group name list =
  testGroup name $
  map (\(name, str, expectedTokens) ->
          testCase name $ th_parse str @?= expectedTokens)
    list

t_parser_basics_hs :: TestTree
t_parser_basics_hs = th_parser_test_group "basics" parserBasicsTests
  
t_parser_precedence_hs :: TestTree
t_parser_precedence_hs = th_parser_test_group "precedence" parserPrecedenceTests


t_parser_basics_js ::TestTree
t_parser_basics_js = testGroup "parser" $
    map (\(name, str, expectedTokens) ->
            jsTestScript name jsExe jsSrcDir "ParserTestEntry" "_parse"
            str expectedTokens)
      parserBasicsTests
      




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
  
-- FIX: quickcheck fuzz for math, checked against external evaluation
-- for external evaluation, maybe use node.js?

-- th_qce = (head . fst . runString)

-- data MathExpression = MathExpression String

-- instance Arbitrary MathExpression where
--   arbitrary = return $ MathExpression "1"

-- hsEval :: String -> IO Double
-- hsEval expr = do
--   mHSEval <- eval expr []
--   case mHSEval of
--     Just [x] -> return $ x
--     Nothing  -> error "couldn't eval"


-- t_eval_qc = testGroup "evaluator quickcheck test"
--   [  testProperty "haskell eval" $ 
--        \(MathExpression expr) -> hsEval expr >>= (\v -> return $ th_qce expr)
--   ]

-- t_eval_vals = testGroup "simple values"
--   [ testCase "single char" $ th_evals "a" @?= [0]
