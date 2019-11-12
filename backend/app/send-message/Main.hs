import           Coach.Messaging    (auditAndText', getTwilioEnv)

import           Data.Hourglass     (localTimeUnwrap)
import           Data.Text          (pack)
import           System.Environment (getArgs, getEnv)
import           System.Hourglass   (localDateCurrent)

main :: IO ()
main = do
  [person, number] <- getArgs
  date <- localTimeUnwrap <$> localDateCurrent
  sheetUrl <- getEnv "SHEET_URL"
  auditAndText' getTwilioEnv (pack number) date (pack person) sheetUrl >>= print
