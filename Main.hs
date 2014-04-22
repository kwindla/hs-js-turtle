module Main
       where

import TurtlePrimitives

main = interact ( unlines . pgmString )
