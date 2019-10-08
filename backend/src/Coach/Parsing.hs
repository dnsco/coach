module Coach.Parsing where

import           Data.Hourglass
import           Data.List       (transpose)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map, fromListWith)
import           Data.Text       (Text, pack)
import           Debug.Trace     (trace)
import           GHC.Unicode     (isSpace)
import qualified Text.CSV        as CSV
import           Text.Parsec     (ParseError)

type CSVResult = Either ParseError PeopleData

type NewCSVResult = Either ParseError [[String]]

type PeopleData = Map Person [Activity]

type Activity = (ActivityName, [Event])

type Event = (Date, Text)

type ActivityName = Text

type Person = Text

parseAndProcess :: String -> String -> CSVResult
parseAndProcess url s =
  parseCSV <$>
  trace ("parsing csv: of " ++ s ++ " from : " ++ url) CSV.parseCSV url s

newProcess :: String -> String -> NewCSVResult
newProcess url s = newParse <$> CSV.parseCSV url s

newParse :: CSV.CSV -> [[String]]
newParse csv = reverseChronological rows
  where
    reverseChronological (names:titles:events) = names : titles : reverse events
    rows = bottomNameFirst <$> rows'
      where
        bottomNameFirst (label:people) = label : reverse people
        rows' = withFirstCell (transpose (withFirstCell csv))
        withFirstCell = filter (not . null . head)

parseCSV :: CSV.CSV -> PeopleData
parseCSV rows =
  fromListWith (++) ((\(p, a) -> (p, [a])) <$> parseActivities rows)

parseActivities :: CSV.CSV -> [(Person, Activity)]
parseActivities rows =
  [(pack p, (pack a, fes es)) | (p:a:es) <- tail rows, not (null p)]
  where
    ds = [d | (Just d) <- parseDate <$> drop 2 (head rows)]
    fes :: [String] -> [Event]
    fes es = [(d, pack e) | (d, e) <- zip ds es]

undoneActivityDays :: DateTime -> Activity -> Bool
undoneActivityDays t as =
  if todHour (dtTime t) > 1 -- hour is zero indexed
    then not . null . dateFinder $ today
    else not . null . dateFinder $
         Date
           { dateYear = dateYear today
           , dateMonth = dateMonth today
           , dateDay = dateDay today - 1
           }
  where
    dateFinder d = [an | (an, es) <- [as], d `notElem` (fst <$> es)]
    today = dtDate t

parseDate :: String -> Maybe Date
parseDate s =
  case read <$> splitOn "/" s of
    [m, d, y] ->
      Just Date {dateDay = d, dateMonth = toEnum (m - 1), dateYear = y}
    _ -> Nothing

trim :: String -> String
trim = (\f -> f . f) (reverse . dropWhile isSpace)
