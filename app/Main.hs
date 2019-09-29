{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Data.ByteString.Lazy.Char8 as Char8
import           Lib
import           Network.Wreq
import           System.Environment
import           Text.CSV


errorIfBlank :: Maybe String -> String
errorIfBlank Nothing  = error "set env SHEET_URL"
errorIfBlank (Just s) = s

main :: IO ()
main = do
  url <- lookupEnv "SHEET_URL"
  r <- get (errorIfBlank url)
  parsed <-
    return (parseCSV (errorIfBlank url) (Char8.unpack $ r ^. responseBody))
  print $ parsed
