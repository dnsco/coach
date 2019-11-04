module Coach.Api where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Hourglass
import           Data.Map.Strict        as Map
import           GHC.Generics
import           Servant
import           System.Environment     (getEnv)
import           System.Hourglass       (localDateCurrent)

import           Coach.Messaging        (runSendMessage)
import           Coach.Network          (parseCsvAt)
import           Coach.Parsing          (delinquentOn)
import qualified Coach.Parsing          as Coach
import           Data.Text              (Text, pack)
import           Text.Parsec            (ParseError)

type PeopleApi = RootEndpoint :<|> SendEndpoint

type RootEndpoint = Get '[ JSON] [Person]

type SendEndpoint = "message" :> Get '[ JSON] String

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
    toApiEvent (date', description) =
      (pack (timePrint ISO8601_Date date'), description)

fetchAndParseForNow :: IO (Either ParseError [Person])
fetchAndParseForNow = do
  csv <- getEnv "SHEET_URL " >>= parseCsvAt
  date <- localTimeUnwrap <$> localDateCurrent
  return (peopleFromDs date <$> csv)

server1 :: Server PeopleApi
server1 = servePeople :<|> makeCall
  where
    servePeople :: Handler [Person]
    servePeople = do
      peopleData <- liftIO fetchAndParseForNow
      case peopleData of
        Right ps -> return ps
        Left _   -> throwError err503 {errBody = "Couldn't parse CSV."}
    makeCall :: Handler String
    makeCall = do
      recipient <- pack <$> liftIO (getEnv "TWILIO_TEST_RECIPIENT")
      message <- liftIO (runSendMessage recipient "YAAASS QUEEEN")
      return (show message)

peopleApi :: Proxy PeopleApi
peopleApi = Proxy

coachApi :: Application
coachApi = serve peopleApi server1
