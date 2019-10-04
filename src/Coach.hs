{-# LANGUAGE ScopedTypeVariables #-}

module Coach
  ( parseAndProcess
  , delinquents
  , Activity
  , CSVResult
  , Delinquents
  , PeopleData
  ) where

import           Coach.Structures
import           Coach.Util
import           Data.Hourglass
import qualified Data.Map          as Map
import           Debug.Trace       (trace)
import qualified Text.CSV          as CSV
import           Text.Parsec.Error (ParseError)

type CSVResult = Either ParseError PeopleData

parseAndProcess :: String -> String -> CSVResult
parseAndProcess url s =
  parseCSV <$>
  trace ("parsing csv: of " ++ s ++ " from : " ++ url) CSV.parseCSV url s

parseCSV :: CSV.CSV -> PeopleData
parseCSV rows =
  Map.fromListWith (++) ((\(p, a) -> (p, [a])) <$> parseActivities rows)

parseActivities :: CSV.CSV -> [(Person, Activity)]
parseActivities rows = [(p, (a, fes es)) | (p:a:es) <- tail rows, not (null p)]
  where
    ds = [d | (Just d) <- parseDate <$> drop 2 (head rows)]
    fes :: [String] -> [Event]
    fes es = [e | e <- zip ds es, not (null (snd e))]

type Delinquents = [(Person, [ActivityName])]

delinquents :: DateTime -> PeopleData -> Delinquents
delinquents d =
  Map.foldrWithKey
    (\k as ps ->
       case fa as of
         []  -> ps
         ans -> ps ++ [(k, ans)])
    []
  where
    fa = undoneActivityDays d

undoneActivityDays :: DateTime -> [Activity] -> [ActivityName]
undoneActivityDays t as =
  if todHour (dtTime t) > 3
    then dateFinder td
    else dateFinder
           Date
             { dateYear = dateYear td
             , dateMonth = dateMonth td
             , dateDay = dateDay td - 1
             }
  where
    dateFinder d = [an | (an, es) <- as, d `notElem` (fst <$> es)]
    td = dtDate t
