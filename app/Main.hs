import           Lib
import           System.Environment

main :: IO ()
main = url >>= parseCsvAt >>= print
  where
    url = errorIfBlank <$> lookupEnv "SHEET_URL"

errorIfBlank :: Maybe String -> String
errorIfBlank Nothing  = error "set env SHEET_URL"
errorIfBlank (Just s) = s
