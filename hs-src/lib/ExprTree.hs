module ExprTree
       where

type ExprList = [ExprTree]
data ExprTree = Assignment Char ExprTree              |
                Defun Char Int ExprTree               |
                TernaryIf ExprTree ExprTree ExprTree  |
                Repeat    ExprTree ExprTree           |
                UnaryOp UnaryFunc ExprTree            |
                BinaryOp BinaryFunc ExprTree ExprTree |
                ConstantNumber Double                 |
                Symbol Char                           |
                Funcall Int Char ExprList             |
                ExprTreeListNode ExprList
              deriving (Show)

data UnaryFunc = UnaryFunc String (Double -> Double)
instance Show UnaryFunc where
  show (UnaryFunc name f) = "(" ++ name ++ ")"
    
data BinaryFunc = BinaryFunc String (Double -> Double -> Double)
instance Show BinaryFunc where
  show (BinaryFunc name f) = "(" ++ name ++ ")"
