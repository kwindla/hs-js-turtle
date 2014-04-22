module TurtlePrimitives
       where

import ExprTree
import SymbolTable
import Tokenizer
import Parser
import Evaluator
import qualified Data.Map as Map
import Control.Monad.State
import Text.Printf

globalTable = SymbolTable (Map.fromList 
  [ ('P', BoundValue pi)
  , ('F', BoundBuiltin 1 (\exprts -> biForward (head exprts)))
  , ('R', BoundBuiltin 1 (\exprts -> biRotate subtract (head exprts)))
  , ('L', BoundBuiltin 1 (\exprts -> biRotate (+) (head exprts)))
  ])

--

runString :: String -> [Double]
runString str =
  evalState (mapM evaluate (((parse globalTable). tokenize) str)) newTSL

pgmString :: String -> [String]
pgmString str =
  let tsl = execState (
        mapM evaluate (((parse globalTable). tokenize) str)) newTSL
  in (view outLines tsl) ++ svgPostlude

--

svgPrelude = ["<svg width=\"200\" height=\"200\">"]
svgPostlude = ["</svg>\n"]

--

newTSL = TSL (Turtle (90.0) (100.0,100.0) (0,0,0)) globalTable svgPrelude 

--

type Point = (Double , Double)
x :: Point -> Double
x p = fst p
y :: Point -> Double
y p = snd p

shp :: (String, String) -> Point -> String
shp n p = printf "%s=\"%.0f\" %s=\"%.0f\"" (fst n) (x p) (snd n) (y p)

vplus :: Point -> Point -> Point
vplus p0 p1 = ((x p0) + (x p1) , (y p0) + (y p1))

vtimes :: Point -> Double -> Point
vtimes p d = ((x p) * d , (y p) * d)

-- turns a heading (in degrees) into a unit vector
directionVectUnit :: Double -> Point
directionVectUnit h =
  let theta = 2 * pi * h / 360
      in (cos(theta) , sin(theta))

directionVect :: Double -> Double -> Point
directionVect h dist = (directionVectUnit h) `vtimes` dist

biForward :: ExprTree -> EvalContext
biForward expr = do
  dist <- evaluate expr
  t <- gets $ view turtle
  let p0 = view pos t
      p1 = p0 `vplus` (directionVect (view heading t) dist)
  modify $ set' turtle pos p1
  appendOutput $ "<line " ++ (shp ("x1","y1") p0) ++ " " ++
    (shp ("x2","y2") p1) ++ " style=\"stroke:rgb(0,0,0);stroke-width:2\" />"
  -- appendOutput ("FORWARD " ++ (showf p0) ++ " -> " ++ (showf p1))
  return dist

biRotate :: (Double -> Double -> Double) -> ExprTree -> EvalContext
biRotate f expr = do
  num <- evaluate expr
  modify $ update' turtle heading (f num)
  return num

--
