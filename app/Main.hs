import           Lib
import           System.Environment

main :: IO ()
main = csv >>= print
  where
    url = errorIfBlank <$> lookupEnv "SHEET_URL"
    csv = url >>= fetchAndParse

errorIfBlank :: Maybe String -> String
errorIfBlank Nothing  = error "set env SHEET_URL"
errorIfBlank (Just s) = s
