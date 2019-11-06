module Coach.Network where

import           Control.Lens               ((<&>), (^.))
import qualified Data.ByteString.Lazy.Char8 as Char8 (unpack)
import           Data.Hourglass             (localTimeUnwrap)
import qualified Network.Wreq               as Wreq
import           System.Hourglass           (localDateCurrent)
import           Text.Parsec                (ParseError)

import           Coach.Parsing

fetchAndParseForNow :: String -> IO (Either ParseError [ApiPerson])
fetchAndParseForNow sheetUrl = do
  csv <- parseCsvAt sheetUrl
  date <- localTimeUnwrap <$> localDateCurrent
  return (peopleFromSheet date <$> csv)

parseCsvAt :: String -> IO CSVResult
parseCsvAt url = fetchUrl url <&> parseAndProcess url

fetchUrl :: String -> IO String
fetchUrl url = body <$> Wreq.get url
  where
    body response = Char8.unpack $ response ^. Wreq.responseBody
