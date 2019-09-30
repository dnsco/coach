module Lib
  ( fetchAndParse
  , processRows
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8 (ByteString, unpack)
import           Data.Maybe                 (catMaybes)
import           Network.Wreq
import           Text.CSV
import           Text.Parsec.Error          (ParseError)

type Activity = (Person, ActivityName, [Event])

type CSVResult = Either ParseError [Activity]

type Person = String

type ActivityName = String

type Date = String

type Event = (Date, String)

fetchAndParse :: String -> IO CSVResult
fetchAndParse url = parser <$> fetchUrl url
  where
    parser = processRows (parseCSV url)

processRows :: (s -> Either ParseError CSV) -> s -> CSVResult
processRows parser s = pure process <*> parsed
  where
    parsed = parser s

process :: CSV -> [Activity]
process rows = processRow . addDates <$> [r | r <- tail rows, not (null (head r))]
  where
    addDates = zip (head rows)

processRow :: [(Field, Field)] -> Activity
processRow ((_, whom):(_, wat):acts) = (whom, wat, [a | a <- acts, not (null (snd a))])

fetchUrl :: String -> IO String
fetchUrl url = rBody <$> get url

rBody :: Response Char8.ByteString -> String
rBody r = Char8.unpack $ r ^. responseBody
