module Lib
  ( fetchAndParse
  ) where

import           Control.Arrow              (left)
import           Control.Lens
import           Data.ByteString.Lazy.Char8 as Char8
import           Network.Wreq
import           Text.CSV

fetchAndParse :: String -> IO (Either String CSV)
fetchAndParse url = left show <$> result
  where
    parser = parseCSV url
    result = parser <$> fetchUrl url

fetchUrl :: String -> IO String
fetchUrl url = rBody <$> get url

rBody :: Response ByteString -> String
rBody r = Char8.unpack $ r ^. responseBody
