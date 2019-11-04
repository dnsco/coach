module Coach.Messaging where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text, pack)
import           System.Environment     (getEnv)
import           Twilio
import           Twilio.Message         (Message)
import           Twilio.Messages

runSendMessage :: Text -> Text -> IO Message
runSendMessage to message =
  runTwilio' (getEnv "TWILIO_ACCOUNT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
    from <- pack <$> liftIO (getEnv "TWILIO_SENDER_NUMBER")
    sendMessage from to message

sendMessage :: Text -> Text -> Text -> Twilio Message
sendMessage from' to' message' = post (PostMessage to' from' message' Nothing)
