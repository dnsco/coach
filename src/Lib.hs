module Lib
  ( parseCsvAt
  , processRows
  , parseAndProcess
  , ParsedCSV
  , Activity
  ) where

import           Control.Arrow              ((>>>))
import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8 (ByteString, unpack)
import           Data.Hourglass
import           Data.List.Split            (splitOn)
import           Data.Map                   as Map (Map, fromList)
import           Network.Wreq
import           Text.CSV
import           Text.Parsec.Error          (ParseError)

parseCsvAt :: String -> IO (Either ParseError ParsedCSV)
parseCsvAt url = fetchUrl url <&> parseAndProcess url

parseAndProcess :: String -> String -> Either ParseError ParsedCSV
parseAndProcess url s = processRows <$> parseCSV url s

type Activity = (ActivityName, [Event])

type ParsedCSV = Map Person [Activity]

processRows :: CSV -> ParsedCSV
processRows rows =
  Map.fromList
    [(p, [(a, completedEvents es)]) | (p:a:es) <- tail rows, not (null p)]
  where
    completedEvents es = [e | e <- zip dates es, not (null (snd e))]
    dates =
      [d | (Just d) <- dateCells <&> (splitOn "/" >>> fmap read >>> toDate)]
    dateCells = drop 2 (head rows)

toDate :: [Int] -> Maybe Date
toDate [m, d, y] =
  Just Date {dateDay = d, dateMonth = toEnum (m - 1), dateYear = y}
toDate _ = Nothing

fetchUrl :: String -> IO String
fetchUrl url = rBody <$> get url

rBody :: Response Char8.ByteString -> String
rBody r = Char8.unpack $ r ^. responseBody

type Person = String

type ActivityName = String

type Event = (Date, String)
