import           Coach.Util         (parseDate)
import           Data.Maybe         (fromJust)
import           Lib
import           System.Environment

main :: IO ()
main = do
  p <- parsed
  print (delinquents (fromJust (parseDate "10/1/2019")) <$> p)
  where
    url = errorIfBlank <$> lookupEnv "SHEET_URL"
    parsed = url >>= parseCsvAt

errorIfBlank :: Maybe String -> String
errorIfBlank Nothing  = error "set env SHEET_URL"
errorIfBlank (Just s) = s
