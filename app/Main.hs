import           Control.Applicative
import           Control.Lens
import           Data.ByteString.Lazy.Char8 as Char8
import           Data.Either
import           Lib
import           Network.Wreq
import           System.Environment
import           Text.CSV

main :: IO ()
main =
  let url = errorIfBlank <$> lookupEnv "SHEET_URL"
      response = rBody <$> (url >>= get)
      csv = parseCSV <$> url <*> response
   in do c <- csv
         print (fromRight [] c)

errorIfBlank :: Maybe String -> String
errorIfBlank Nothing  = error "set env SHEET_URL"
errorIfBlank (Just s) = s

rBody :: Response ByteString -> String
rBody r = Char8.unpack $ r ^. responseBody
