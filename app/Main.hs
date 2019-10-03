
import           Data.Hourglass
import           Lib
import           System.Environment
import           System.Hourglass

main :: IO ()
main = do
  currentDate <- dtDate . localTimeUnwrap <$> localDateCurrent
  people <- parseCsvAt =<< getEnv "SHEET_URL"
  print (delinquents currentDate <$> people)
