module Main where

import Haste
import Haste.Prim
import Haste.Foreign
import TurtlePrimitives

-- Examples:
--   echo "#36{R10#8{F25L45}}" | runghc tinker.hs | display svg:-
--   echo "#8{R45#6{#90{F1R2}R90}}" | runghc tinker.hs | display svg:-
-- 
-- to debug...  - runString "P{A=4 P=2 P+A}PA"
--           or - (parse . tokenize) "a=4 P"


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