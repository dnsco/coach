module Coach.Parsing where

import           Data.Aeson      (ToJSON)
import           Data.Hourglass
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text, pack)
import           GHC.Unicode     (isSpace)

import           GHC.Generics    (Generic)
import qualified Text.CSV        as CSV
import           Text.Parsec     (ParseError)

type CSVResult = Either ParseError PeopleData

type PeopleData = Map.Map Person [Activity]

type Activity = (ActivityName, [Event])

type Event = (Date, Text)

type ActivityName = Text

type Person = Text

data ApiPerson =
  ApiPerson
    { name       :: Text
    , activities :: [ApiActivity]
    }
  deriving (Eq, Show, Generic)

instance ToJSON ApiPerson

data ApiActivity =
  ApiActivity
    { title        :: Text
    , isDelinquent :: Bool
    , events       :: [(Text, Text)]
    }
  deriving (Eq, Show, Generic)

instance ToJSON ApiActivity

parseAndProcess :: String -> String -> CSVResult
parseAndProcess url s = parseCSV <$> CSV.parseCSV url s

parseCSV :: CSV.CSV -> PeopleData
parseCSV rows =
  Map.fromListWith (++) ((\(p, a) -> (p, [a])) <$> parseActivities rows)

parseActivities :: CSV.CSV -> [(Person, Activity)]
parseActivities rows =
  [(pack p, (pack a, fes es)) | (p:a:es) <- tail rows, not (null p)]
  where
    ds = [d | (Just d) <- parseDate <$> drop 2 (head rows)]
    fes :: [String] -> [Event]
    fes es = [(d, pack e) | (d, e) <- zip ds es]

parseDate :: String -> Maybe Date
parseDate s =
  case read <$> splitOn "/" s of
    [m, d, y] ->
      Just Date {dateDay = d, dateMonth = toEnum (m - 1), dateYear = y}
    _ -> Nothing

trim :: String -> String
trim = (\f -> f . f) (reverse . dropWhile isSpace)

delinquentOn :: DateTime -> [Event] -> Bool
delinquentOn now es =
  if todHour (dtTime now) > 1 -- hour is zero indexed, so padding till 2am
    then dateFinder today
    else dateFinder yesterday
  where
    today = dtDate now
    yesterday = today {dateDay = dateDay today - 1}
    dateFinder d = d `notElem` (fst <$> es)

peopleFromSheet :: DateTime -> PeopleData -> [ApiPerson]
peopleFromSheet date = Map.foldlWithKey (\ps k as -> ps ++ [toApi date k as]) []

toApi :: DateTime -> Person -> [Activity] -> ApiPerson
toApi date k as = ApiPerson k (toApiActivity <$> as)
  where
    toApiActivity :: Activity -> ApiActivity
    toApiActivity (an, es) =
      ApiActivity an (delinquentOn date es) (reverse (toApiEvent <$> es))
    toApiEvent :: Event -> (Text, Text)
    toApiEvent (date', description) =
      (pack (timePrint ISO8601_Date date'), description)
