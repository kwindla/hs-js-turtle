
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
import TestsForEvaluator

-- javascript options (FIX: make these cmd line configurable) --

jsExe = "node"
jsSrcDir = "../js-src"

-- top-level test groupings --
-- 
-- from within the tests director run tests like so:
--   runghc -i../hs-src/lib test.hs 
--
-- or, to run just haskell or just javascript tests
--
--   runghc -i../hs-src/lib test.hs -p haskell
--   runghc -i../hs-src/lib test.hs -p javascript

main = defaultMain $ testGroup "all" [t_haskell, t_javascript]

t_haskell = testGroup "haskell" [ t_tokenizer_hs
                                , t_parser_hs
                                , t_evaluator_hs
                                ]

t_javascript = testGroup "javascript" [ t_tokenizer_js
                                      , t_parser_js
                                      , t_evaluator_js
                                      ]



-- tokenizer tests --

t_tokenizer_hs = th_testgroup_hs tokenize "tokenizer" tokenizerTests

t_tokenizer_js = th_testgroup_js "Tokenizer" "tokenize" "tokenizer"
                   tokenizerTests


-- parser tests --

parserTestGroups = [ ("basics", parserBasicsTests)
                   , ("precedence", parserPrecedenceTests) ]

t_parser_hs = testGroup "parser" $
  map (\(name,testList) ->
           th_testgroup_hs (parse globalTable . tokenize) name testList)
    parserTestGroups


t_parser_js = testGroup "parser" $
  map (\(name,testList) ->
           th_testgroup_js "ParserTestEntry" "_parse" name testList)
    parserTestGroups


-- evaluator tests

evaluatorTestGroups = [ ("simple values", evaluatorSimpleValues)
                      , ("scope", evaluatorScope) ]

t_evaluator_hs = testGroup "evaluator" $
  map (\(name,testList) ->
           th_testgroup_hs (fst . runString) name testList)
    evaluatorTestGroups

t_evaluator_js = testGroup "evaluator" $
  map (\(name,testList) ->
           th_testgroup_js "TurtleSVG" "runProgramValues" name testList)
    evaluatorTestGroups



-- util functions --

th_testgroup_hs f name list =
  testGroup name $
  map (\(name, str, expectedTokens) ->
           testCase name $ f str @?= expectedTokens)
    list
th_testgroup_js jsModule f name list = testGroup name $
  map (\(name, str, expectedTokens) ->
           jsTestScript name jsExe jsSrcDir jsModule f str expectedTokens)
    list



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
