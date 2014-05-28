{-# LANGUAGE DeriveDataTypeable #-}

-- written with the help of
--   https://github.com/jstolarek/tasty-program/blob/master/src/Test/Tasty/Program.hs
-- FIX: go back and document in the style of the above 

module TestNodeProgram where

import Data.Typeable
import Test.Tasty.Providers
import System.Process
import Data.String.Utils


data TestScript a =
  TS String String String String String a
  deriving (Typeable)

-- node "-e" "var _hsTest=require('<module>');
--            console.log(hsTest.<function>('<script>'));"
jsTestScript :: (Typeable a, Eq a, Read a, Show a) =>
                TestName  -- name
             -> String    -- node executable
             -> String    -- js/node modules director
             -> String    -- js module to require()
             -> String    -- function to call
             -> String    -- script to pass as arg to the function
             -> a         -- result value
             -> TestTree
jsTestScript testName cmd jsModulesDir jsModule function expression expected =
  singleTest testName
             (TS cmd jsModulesDir jsModule function expression expected)

instance (Typeable a, Eq a, Read a, Show a) => IsTest (TestScript a) where
  run _ (TS cmd jsMD jsM f expr expected) _ = do
    actual <- runNodeScript cmd jsMD jsM f expr
    if (read actual) == expected
      then return $ testPassed ""
      else return $ testFailed (
        "got:                        " ++ actual ++
        "but expected approximately: " ++ (show expected))
  testOptions = return []

runNodeScript :: String -> String -> String -> String -> String -> IO String
runNodeScript cmd jsModulesDir jsModule function expression =
  readProcess cmd
    [ "-e"
    , "var _hsTest=require('" ++ jsModulesDir ++ "/" ++ jsModule ++ "'); " ++
      "console.log(_hsTest." ++ function ++ "('" ++ 
        -- ((unpack . bytes . sh . pack) expression)
        (replace "\\" "\\\\" expression)
      ++ "'));"
      ] []
