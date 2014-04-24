module Main
       where

import System.Environment
import Control.Monad
import TurtlePrimitives

main = do
  pgm <- liftM head getArgs
  -- print $ (unlines . pgmString) pgm
  print $ runString pgm
  
  
  -- interact ( unlines . pgmString )
