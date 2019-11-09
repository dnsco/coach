module Coach.Messaging where

import           Data.Hourglass     (DateTime)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text, intercalate, pack)
import           System.Environment (getEnv)
import           Twilio
import           Twilio.Message     (Message)
import           Twilio.Messages

import           Coach.Network
import           Coach.Parsing
import           Debug.Trace        (trace)

auditAndText' ::
     IO TwilioEnv -> DateTime -> Person -> String -> IO (Maybe Message)
auditAndText' env now person sheetUrl =
  parseCsvAt sheetUrl >>= \case
    Left _ -> return Nothing
    Right ps' -> auditAndText env now person ps'

auditAndText ::
     IO TwilioEnv -> DateTime -> Person -> PeopleData -> IO (Maybe Message)
auditAndText env now person people =
  case Map.lookup person people of
    Just as -> messageIfDelinquent as
    Nothing -> return Nothing
  where
    messageIfDelinquent as =
      case delinquentActivities as of
        []  -> return Nothing
        as' -> trace (show as') Just <$> sendMessage env (messageText as')
    delinquentActivities as =
      filter isDelinquent (activities (toApi now person as))
    messageText as =
      "step up your game with " <> intercalate ", " (title <$> as)

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
