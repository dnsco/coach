module Coach.Messaging where

import           Data.Text          (Text, pack)
import           System.Environment (getEnv)
import           Twilio
import           Twilio.Message     (Message)
import           Twilio.Messages

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

runSendMessage :: IO TwilioEnv -> Text -> IO Message
runSendMessage env message = do
  let twilio = runTwilio' (twiSid <$> env) (twiToken <$> env)
  from <- pack . twiSender <$> env
  to <- pack . twiRecipient <$> env
  twilio $ sendMessage from to message

sendMessage :: Text -> Text -> Text -> Twilio Message
sendMessage from to message = post $ PostMessage to from message Nothing
