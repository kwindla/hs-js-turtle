
module TestsForParser
  where

import ExprTree

parserBasicsTests =
  [ ("single char", "a", [Symbol 'a'])
  , ("three chars", "abc", [Symbol 'a', Symbol 'b', Symbol 'c'])
    
  , ("assignment", "a=a+1",
     [Assignment 'a' (BinaryOp (read "(+)") (Symbol 'a') (ConstantNumber 1.0))])

  , ("plus", "a+3", [BinaryOp (read "(+)") (Symbol 'a') (ConstantNumber 3.0)])
  , ("minus", "a-3", [BinaryOp (read "(-)") (Symbol 'a') (ConstantNumber 3.0)])
  , ("times", "a*3", [BinaryOp (read "(*)") (Symbol 'a') (ConstantNumber 3.0)])
  , ("div", "a/3", [BinaryOp (read "(/)") (Symbol 'a') (ConstantNumber 3.0)])
  , ("mod", "a\\3", [BinaryOp (read "(\\)") (Symbol 'a') (ConstantNumber 3.0)])

  , ("equals", "a~3",
       [BinaryOp (read "(=)") (Symbol 'a') (ConstantNumber 3.0)])

  , ("greater than", "a>3",
       [BinaryOp (read "(>)") (Symbol 'a') (ConstantNumber 3.0)])
  , ("less than", "a<3",
       [BinaryOp (read "(<)") (Symbol 'a') (ConstantNumber 3.0)])
      
  , ("unary -", "-1", [UnaryOp (read "(-)") (ConstantNumber 1.0)])
  , ("unary +", "+1", [(ConstantNumber 1.0)])

    
  , ("a{bc}", "a{bc}", [Symbol 'a', ExprTreeListNode [Symbol 'b',Symbol 'c']])
    
  , ("ternary if", "?a>bcd",
       [TernaryIf (BinaryOp (read "(>)") (Symbol 'a') (Symbol 'b'))
        (Symbol 'c') (Symbol 'd')])

  , ("repeat", "#a/3x",
       [Repeat (BinaryOp (read "(/)") (Symbol 'a') (ConstantNumber 3.0))
         (Symbol 'x')])
      
  , ("defun and funcall", "&S2a Sbc",
       [Defun 'S' 2 (Symbol 'a'), Funcall 2 'S' [Symbol 'b',Symbol 'c']])
  ]

parserCommaTests =
  [ ("nums", "1,2,-3", [(ConstantNumber 1.0), (ConstantNumber 2.0),
                        (UnaryOp (read "(-)") (ConstantNumber 3.0))])

  , ("funcall", "&s2{a+b}s2,-(3+4)", 
      [(Defun 's' 2
        (ExprTreeListNode [(BinaryOp (read "(+)") (Symbol 'a') (Symbol 'b'))])),
        (Funcall 2 's' [(ConstantNumber 2.0), (UnaryOp (read "(-)") 
          (BinaryOp (read "(+)") (ConstantNumber 3.0) (ConstantNumber 4.0)))])])

  ]

parserPrecedenceTests =
  [ ("+*", "a+1*3", [BinaryOp (read "(+)") (Symbol 'a')
       (BinaryOp (read "(*)") (ConstantNumber 1.0) (ConstantNumber 3.0))])
    
  , ("-/", "a-1/3", [BinaryOp (read "(-)") (Symbol 'a')
        (BinaryOp (read "(/)") (ConstantNumber 1.0) (ConstantNumber 3.0))])
      
  , (">", "a+1>4*3", [BinaryOp (read "(>)")
        (BinaryOp (read "(+)") (Symbol 'a') (ConstantNumber 1.0))
        (BinaryOp (read "(*)") (ConstantNumber 4.0) (ConstantNumber 3.0))])
      
  , ("*+", "a*1+3", [BinaryOp (read "(+)")
         (BinaryOp (read "(*)") (Symbol 'a') (ConstantNumber 1.0))
         (ConstantNumber 3.0)])

  , ("*(+)", "a*(1+3)", [BinaryOp (read "(*)")
          (Symbol 'a')
          (BinaryOp (read "(+)") (ConstantNumber 1.0) (ConstantNumber 3.0))])
  ]

-- FIX: add test for proper arg count for lexically scoped defuns that
-- share a name

