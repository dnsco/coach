module Coach.Api where

import           Control.Monad.IO.Class (liftIO)
import           Servant
import           System.Environment     (getEnv)

import           Coach.Messaging        (getTwilioEnv, sendMessage)
import           Coach.Network          (fetchAndParseForNow)
import           Coach.Parsing          (ApiPerson)
import           Data.Text              (pack)

type PeopleApi = RootEndpoint :<|> SendEndpoint

type RootEndpoint = Get '[ JSON] [ApiPerson]

type SendEndpoint = "message" :> Get '[ JSON] String

server1 :: Server PeopleApi
server1 = servePeople :<|> liftIO makeCall
  where
    servePeople = do
      peopleData <- liftIO $ getEnv "SHEET_URL" >>= fetchAndParseForNow
      case peopleData of
        Right ps -> return ps
        Left _   -> throwError err503 {errBody = "Couldn't parse CSV."}
    makeCall = do
      recipient <- pack <$> getEnv "TWILIO_TEST_RECIPIENT"
      show <$> sendMessage getTwilioEnv recipient "YAAASS QUEEEN"

peopleApi :: Proxy PeopleApi
peopleApi = Proxy

coachApi :: Application
coachApi = serve peopleApi server1
