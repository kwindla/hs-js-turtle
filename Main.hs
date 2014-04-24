module Main
       where

import System.Environment
import Control.Monad
import TurtlePrimitives

-- #10{R45#5{#90{F1R2}R90}}

main = do
  pgm <- liftM head getArgs
  putStrLn $ (unlines . pgmString) pgm
  -- print $ runString pgm
  
  
  -- interact ( unlines . pgmString )
