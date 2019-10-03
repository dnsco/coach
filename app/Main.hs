import           Data.Hourglass
import           Data.Monoid        (mconcat)
import           Data.String        (fromString)
import           Data.Text.Lazy     (Text)
import           Lib
import           System.Environment
import           System.Hourglass
import           Web.Scotty

main :: IO ()
main = do
  port <- getEnv "PORT"
  sheetUrl <- getEnv "SHEET_URL"
  currentDate <- dtDate . localTimeUnwrap <$> localDateCurrent
  people <- parseCsvAt sheetUrl
  scotty (read port) $ do get "/" $ do html $ mconcat [fromString (show (delinquents currentDate <$> people)) :: Text]
