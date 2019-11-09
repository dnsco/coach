module MessagingSpec
  ( spec
  ) where

import           Data.Hourglass
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromJust, isJust)

import           Coach.Messaging
import           Coach.Parsing      (parseDate)
import           System.Environment (getEnv)
import           Test.Hspec

spec :: Spec
spec = do
  let now = DateTime today (TimeOfDay 19 0 0 0)
      today = fromJust $ parseDate "11/06/2019"
      yesterday = fromJust $ parseDate "11/05/2019"
      delinquentActivity = ("Climbing", [(yesterday, "NIAD")])
      doneActivity =
        ( "Surfing"
        , [(yesterday, "OB ankle biters"), (today, "brolinas with scotty")])
      twilio = do
        sid <- getEnv "TWILIO_TEST_SID"
        token <- getEnv "TWILIO_TEST_TOKEN"
        sender <- getEnv "TWILIO_TEST_SENDER"
        recipient <- getEnv "TWILIO_TEST_RECIPIENT"
        return $ TwilioEnv sid token sender recipient
  describe "Auditing a person" $ do
    context "When they have delinquent activities" $ do
      let people = Map.singleton "Denny" [delinquentActivity, doneActivity]
      it "sends a text" $ do
        response <- auditAndText twilio now "Denny" people
        response `shouldSatisfy` isJust
    context "When they are all good" $ do
      let people = Map.singleton "Denny" [doneActivity]
      it "doesn't send a text" $ do
        let response = auditAndText twilio now "Denny" people
        response `shouldReturn` Nothing
