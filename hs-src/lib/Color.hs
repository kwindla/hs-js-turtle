{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Color
       where


import Data.Aeson
import GHC.Generics (Generic)
import Control.Applicative
import qualified Data.ByteString.Lazy as B


data Color = Color { r :: Double
                   , g :: Double
                   , b :: Double
                   } deriving (Show, Generic)

instance FromJSON Color

jsonFile :: FilePath
jsonFile = "../../js-src/colors.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Color])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
