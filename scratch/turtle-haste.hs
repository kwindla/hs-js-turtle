module Main where

import Haste
import Haste.Prim
import Haste.Foreign
import Parser
import TurtlePrimitives

-- Examples:
--     #36{R10#8{F25L45}}
--     #8{R45#6{#90{F1R2}R90}}
-- 

jsRunStr :: String -> IO String
jsRunStr str = do
  return $ (unlines . pgmString) str
  
main = do
  export (toJSStr "jsRunStr") jsRunStr
  writeLog ("ready")
  withElems ["program", "output"] doProgram

doProgram [programTextArea, output] = do
    onEvent programTextArea OnChange $ showOutput
  where
    showOutput = do
      -- writeLog ("in show output")
      mstr <- getValue programTextArea
      case mstr of
        Just str -> setProp output "innerHTML" $ (unlines . pgmString) str
        _        -> return ()    