module Main
       where

import System.Environment
import System.Console.GetOpt
import Control.Monad
import Data.List

import TurtlePrimitives


-- #10{R45#5{#90{F1R2}R90}}


  
main = do
  args <- getArgs
  (options, argv') <- cmdLineOptions args
  let (values, svg) = runString (head argv') -- FIX: error check
  let str =
        if Values `elem` options
        then show values else ""
      str' = if (not (Values `elem` options)) || (Svg `elem` options)
             then (str ++ "\n" ++ svg) else str
  putStrLn str'


data Flag = Svg | Values deriving (Show, Eq)
               
options :: [OptDescr Flag]
options =
  [ Option ['s'] ["svg"] (NoArg Svg) "print out SVG (defaults to true)"
  , Option ['n'] ["numbers"] (NoArg Values)
    "print out an array of expression return values"
  ]

cmdLineOptions :: [String] -> IO ([Flag], [String])
cmdLineOptions argv =
  case getOpt Permute options argv of
    (options, argv',[]) -> return (options, argv')
    (_,_,errors) -> ioError (userError (concat errors ++
                                        usageInfo header options))
  where header = "Usage: turtle [OPTION...] program-string"



