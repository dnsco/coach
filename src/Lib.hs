module Lib
  ( parseCsvAt
  , module Coach
  ) where

import           Coach
import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8 (ByteString, unpack)
import           Network.Wreq

parseCsvAt :: String -> IO CSVResult
parseCsvAt url = fetchUrl url <&> parseAndProcess url

fetchUrl :: String -> IO String
fetchUrl url = rBody <$> get url

rBody :: Response Char8.ByteString -> String
rBody r = Char8.unpack $ r ^. responseBody
