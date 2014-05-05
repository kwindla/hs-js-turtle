{-# LANGUAGE DeriveDataTypeable #-}

module ExprTree
       where

import Data.Typeable  -- used by our custom Test.Hasty node testrunner
import Text.Read


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
              deriving (Show, Eq, Read, Typeable)

data UnaryFunc = UnaryFunc String (Double -> Double)

instance Show UnaryFunc where
  show (UnaryFunc name f) = "(" ++ name ++ ")"

instance Eq UnaryFunc where
  (UnaryFunc namel _) == (UnaryFunc namer _) = namel == namer
  
instance Read UnaryFunc where
  readsPrec _ value = tryParse value [ ("(+)", UnaryFunc "+" undefined)
                                     , ("(-)", UnaryFunc "-" undefined)
                                     ]

data BinaryFunc = BinaryFunc String (Double -> Double -> Double)

instance Show BinaryFunc where
  show (BinaryFunc name f) = "(" ++ name ++ ")"

instance Eq BinaryFunc where
  (BinaryFunc namel _) == (BinaryFunc namer _) = namel == namer
  
instance Read BinaryFunc where
  readsPrec _ value = tryParse value [ ("(+)", BinaryFunc "+" undefined)
                                     , ("(-)", BinaryFunc "-" undefined)
                                     , ("(*)", BinaryFunc "*" undefined) 
                                     , ("(/)", BinaryFunc "/" undefined)
                                     , ("(>)", BinaryFunc ">" undefined)
                                     , ("(<)", BinaryFunc "<" undefined)
                                     ]


tryParse _ [] = []
tryParse value ((attempt, result):xs) =
  let trimmed = dropWhile (\c -> c == ' ') value
  in if (take (length attempt) trimmed) == attempt
     then [(result, drop (length attempt) trimmed)]
     else tryParse value xs
