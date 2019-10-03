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
import qualified Text.CSV          as CSV
import           Text.Parsec.Error (ParseError)

type CSVResult = Either ParseError PeopleData

parseAndProcess :: String -> String -> CSVResult
parseAndProcess url s = parseCSV <$> CSV.parseCSV url s

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

delinquents :: Date -> PeopleData -> Delinquents
delinquents d =
  Map.foldrWithKey
    (\k as ps ->
       case fa as of
         []  -> ps
         ans -> ps ++ [(k, ans)])
    []
  where
    fa = undoneActivityDays d

undoneActivityDays :: Date -> [Activity] -> [ActivityName]
undoneActivityDays d as = [an | (an, es) <- as, d `notElem` (fst <$> es)]
