module Lib
  ( fetchAndParse
  , processRows
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8 (ByteString, unpack)
import           Network.Wreq
import           Text.CSV
import           Text.Parsec.Error          (ParseError)

type Activity = (Person, ActivityName, [Event])

type CSVResult = Either ParseError ParsedCSV

fetchAndParse :: String -> IO CSVResult
fetchAndParse url = parser <$> fetchUrl url
  where
    parser = processRows (parseCSV url)

processRows :: (s -> Either ParseError CSV) -> s -> CSVResult
processRows parser s = pure process <*> parsed
  where
    parsed = parser s

type ParsedCSV = [Activity]

process :: CSV -> ParsedCSV
process rows =
  [(p, a, completedEvents es) | (p:a:es) <- tail rows, not (null p)]
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
