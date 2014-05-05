
module TestsForEvaluator
  where

import Evaluator

evaluatorSimpleValues :: [ (String, String, [Double]) ]
evaluatorSimpleValues =
  [ ("single char", "a", [0])
  , ("single number", "12347", [12347])
  , ("three simple values", "a 1 2", [0,1,2])
  , ("assignment", "a=23 a", [23, 23])
  , ("defun", "&f2{a}", [2])
  , ("if (t)", "?1 100 200", [100])
  , ("if (f)", "?0 100 200", [200])
  , ("if (t from var)", "a=1 ?a 100 200", [1,100])
  , ("if (f from var)", "a=0 ?a 100 200", [0,200])
  , ("repeat", "a=1 #4{a=a+1}", [1,5])
  , ("unary +", "a=1 (+a)", [1,1])
  , ("unary -", "a=1 (-a)", [1,-1])
  , ("funcall", "a=1 &f0a f", [1,0,1])
  , ("{list}", "{1 2 3 4}", [4])
  ]


evaluatorScope :: [ (String, String, [Double]) ]
evaluatorScope =
  [ ("basic block", "{a=23} a", [23, 0])
  , ("lexical inherit", "a=0 {a=23} a", [0, 23, 23])
  , ("defun", "&f0{a} a=23 f", [0, 23, 0])

  , ("lexical x 2", "a=10 {a=12 {a=23} 17} a", [10, 17, 23])
    
  , ("closure r", "a=12 &f0{a} a=23 f a", [12, 0, 23, 23, 23])
  , ("closure w-1", "a=12 &f0{a=a+1} a=23 f a", [12, 0, 23, 24, 24])
  , ("closure w-2", "a=12 &f0{a=a+1} f a", [12, 0, 13, 13])
  , ("  ...", "&f0{a=a+1} a=23 f a", [0, 23, 1, 23])
      
  , ("# funcall", "&f0{a=a+1} #10f a", [0, 1, 0])
  , ("  ... closure", "a=1 &f0{a=a+1} #10f a", [1, 0, 11, 11])

  , ("# nested", "#10#10a=a+1 a", [100, 100])
  , ("# nested block w/ local", "#10{#10{a=a+1}} a", [10, 0])
  , ("# nested block w/ lexical inherit", "a=100 #10{#10{a=a+1}} a",
      [100, 200, 200])

  , ("funcall 1 arg", "&f1{a} f23", [1, 23])
  , ("  ... closure", "a=100 &f1{a} f23", [100, 1, 23])
  , ("funcall 2 args", "&f2{a+b} f23 24", [2, 47])
  , ("  ... closure", "a=100 b=10 &f2{a+b} f23 24 a b",
      [100, 10, 2, 47, 100, 10])
 
  , ("  ... w", "a=100 b=10 &f2{a+b} a=12 b=13 f23 24 a b",
      [100, 10, 2, 12, 13, 47, 12, 13])

  , ("three args + closure", "d=100 &f3{a+b+c+d} f1 2 3 a b c d",
      [100, 3, 106, 0, 0, 0, 100])
  ]
