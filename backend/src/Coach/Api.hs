module Coach.Api where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Hourglass         (DateTime, localTimeUnwrap)
import           Data.Map.Strict        as Map
import           GHC.Generics
import           Servant
import           System.Environment     (getEnv)
import           System.Hourglass       (localDateCurrent)

import           Coach.Network          (newParseCsv, parseCsvAt)
import           Coach.Parsing          (delinquentOn)
import qualified Coach.Parsing          as Coach
import           Data.Text              (Text, pack)

type PeopleApi = Get '[ JSON] [Person] :<|> "people" :> Get '[ JSON] [[String]]

data Person =
  Person
    { name       :: Text
    , activities :: [Activity]
    }
  deriving (Eq, Show, Generic)

instance ToJSON Person

data Activity =
  Activity
    { title        :: Text
    , isDelinquent :: Bool
    , events       :: [(Text, Text)]
    }
  deriving (Eq, Show, Generic)

instance ToJSON Activity

peopleFromDs :: DateTime -> Coach.PeopleData -> [Person]
peopleFromDs date = Map.foldlWithKey (\ps k as -> ps ++ [toApi k as date]) []

toApi :: Coach.Person -> [Coach.Activity] -> DateTime -> Person
toApi k as date = Person k (toApiActivity <$> as)
  where
    toApiActivity :: Coach.Activity -> Activity
    toApiActivity (an, es) =
      Activity an (delinquentOn date es) (reverse (toApiEvent <$> es))
    toApiEvent :: Coach.Event -> (Text, Text)
    toApiEvent (date', description) = (pack (show date'), description)

server1 :: Server PeopleApi
server1 =
  do sheetUrl <- liftIO $ getEnv "SHEET_URL"
     peopleData <- liftIO $ parseCsvAt sheetUrl
     currentDate <- liftIO (localTimeUnwrap <$> localDateCurrent)
     case peopleData of
       Right ps -> return (peopleFromDs currentDate ps)
       Left _   -> throwError err503 {errBody = "Couldn't parse CSV."}
     :<|> do
    sheetUrl <- liftIO $ getEnv "SHEET_URL"
    people <- liftIO $ newParseCsv sheetUrl
    case people of
      Right ps -> return ps
      Left _   -> throwError err503 {errBody = "Couldn't parse CSV."}

peopleApi :: Proxy PeopleApi
peopleApi = Proxy

coachApi :: Application
coachApi = serve peopleApi server1
