module Coach.Util where

import           Data.Hourglass
import           Data.List.Split (splitOn)
import           GHC.Unicode     (isSpace)

parseDate :: String -> Maybe Date
parseDate s =
  case read <$> splitOn "/" s of
    [m, d, y] ->
      Just Date {dateDay = d, dateMonth = toEnum (m - 1), dateYear = y}
    _ -> Nothing

trim :: String -> String
trim = (\f -> f . f) (reverse . dropWhile isSpace)
