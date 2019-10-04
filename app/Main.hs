import           Coach.Web              (dqsResponse)
import           Control.Monad.IO.Class
import           Data.Hourglass
import           Data.Monoid            (mconcat)
import           Data.String            (fromString)
import           Lib
import           System.Environment
import           System.Hourglass
import           Web.Scotty

main :: IO ()
main = do
  port <- getEnv "PORT"
  sheetUrl <- getEnv "SHEET_URL"
  scotty (read port) $ do
    get "/" $ do
      people <- liftIO $ parseCsvAt sheetUrl
      currentDate <- liftIO $ localTimeUnwrap <$> localDateCurrent
      let res = dqsResponse currentDate people
      html $ mconcat [fromString res]
