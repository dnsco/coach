module Coach.Messaging where

import           Data.Hourglass     (DateTime)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromJust)
import           Data.Text          (Text, intercalate, pack)
import           System.Environment (getEnv)
import           Twilio             (Twilio, runTwilio')
import           Twilio.Message     (Message)
import           Twilio.Messages    (PostMessage (..), post)

import           Coach.Network
import           Coach.Parsing

auditAndText' ::
     IO TwilioEnv -> DateTime -> Person -> String -> IO (Maybe Message)
auditAndText' env now person sheetUrl =
  parseCsvAt sheetUrl >>= \case
    Left e -> error $ "failed to parse csv at " <> sheetUrl <> ": " <> show e
    Right ps' -> auditAndText env now person ps'

auditAndText ::
     IO TwilioEnv -> DateTime -> Person -> PeopleData -> IO (Maybe Message)
auditAndText env now person people =
  case delinquentActivities $ fromJust (Map.lookup person people) of
    [] -> return Nothing
    as -> Just <$> sendMessage env (messageText as)
  where
    delinquentActivities as = filter isDelinquent (activities' as)
    activities' as = activities $ toApi now person as
    messageText as' =
      "step up your game with " <> intercalate ", " (title <$> as')

getTwilioEnv :: IO TwilioEnv
getTwilioEnv = do
  sid <- getEnv "TWILIO_ACCOUNT_SID"
  token <- getEnv "TWILIO_AUTH_TOKEN"
  sender <- getEnv "TWILIO_SENDER_NUMBER"
  recipient <- getEnv "TWILIO_TEST_RECIPIENT"
  return $ TwilioEnv sid token sender recipient

data TwilioEnv =
  TwilioEnv
    { twiSid       :: String
    , twiToken     :: String
    , twiSender    :: String
    , twiRecipient :: String
    }

sendMessage :: IO TwilioEnv -> Text -> IO Message
sendMessage env body = do
  let twilio = runTwilio' (twiSid <$> env) (twiToken <$> env)
  from <- pack . twiSender <$> env
  to <- pack . twiRecipient <$> env
  twilio $ postMessage from to body

postMessage :: Text -> Text -> Text -> Twilio Message
postMessage from to message = post $ PostMessage to from message Nothing
