module Coach
  ( parseAndProcess
  , delinquents
  , Activity
  , CSVResult
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
parseAndProcess url s = parseCSV <$> CSV.parseCSV url s

parseCSV :: CSV.CSV -> PeopleData
parseCSV rows =
  Map.fromList
    [(p, [(a, completedEvents es)]) | (p:a:es) <- tail rows, not (null p)]
  where
    completedEvents es = [e | e <- zip dates es, not (null (snd e))]
    dates = [d | (Just d) <- parseDate <$> drop 2 (head rows)]

delinquents :: Date -> PeopleData -> [Person]
delinquents d pd =
  [p | (p, as) <- Map.toList pd, not (null (activityFilter as))]
  where
    activityFilter as = [an | (an, es) <- as, d `notElem` (fst <$> es)]
