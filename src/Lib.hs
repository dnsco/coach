module Lib
  ( parseCsvAt
  , processRows
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8 (ByteString, unpack)
import           Data.Map                   as Map (Map, fromList)
import           Network.Wreq
import           Text.CSV
import           Text.Parsec.Error          (ParseError)

type CSVResult = Either ParseError ParsedCSV

parseCsvAt :: String -> IO CSVResult
parseCsvAt url = parser <$> fetchUrl url
  where
    parser = processRows (parseCSV url)

processRows :: (s -> Either ParseError CSV) -> s -> CSVResult
processRows parser s = pure process <*> parsed
  where
    parsed = parser s

type Activity = (ActivityName, [Event])

type ParsedCSV = Map Person [Activity]

process :: CSV -> ParsedCSV
process rows = Map.fromList [(p, [(a, completedEvents es)]) | (p:a:es) <- tail rows, not (null p)]
  where
    completedEvents es = [e | e <- zip dates es, not (null (snd e))]
    dates = drop 2 (head rows)

fetchUrl :: String -> IO String
fetchUrl url = rBody <$> get url

rBody :: Response Char8.ByteString -> String
rBody r = Char8.unpack $ r ^. responseBody

type Person = String

type ActivityName = String

type Date = String

type Event = (Date, String)
