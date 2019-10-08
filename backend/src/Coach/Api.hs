module Coach.Api where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Hourglass
import           Data.Map.Strict        as Map
import           Data.Text
import           GHC.Generics
import           Servant
import           System.Environment     (getEnv)
import           System.Hourglass

import           Coach.Network          (newParseCsv, parseCsvAt)
import           Coach.Parsing          (delinquents)
import qualified Coach.Structures       as Coach

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

peopleFromDs :: Coach.Delinquents -> [Person]
peopleFromDs = Map.foldlWithKey (\ps k as -> ps ++ [toApi k as]) []

toApi :: Coach.Person -> [Coach.DActivity] -> Person
toApi k as = Person k (toApiActivity <$> as)
  where
    toApiActivity :: Coach.DActivity -> Activity
    toApiActivity (an, isD, es) = Activity an isD (toApiEvent <$> es)
    toApiEvent :: Coach.Event -> (Text, Text)
    toApiEvent (date, description) = (pack (show date), description)

server1 :: Server PeopleApi
server1 =
  do sheetUrl <- liftIO $ getEnv "SHEET_URL"
     currentDate <- liftIO (localTimeUnwrap <$> localDateCurrent)
     peopleData <- liftIO $ parseCsvAt sheetUrl
     case peopleData of
       Right peopleData ->
         return . peopleFromDs $ delinquents currentDate peopleData
       Left _ -> throwError err503 {errBody = "Couldn't parse CSV."}
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
