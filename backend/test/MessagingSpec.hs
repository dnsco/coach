module MessagingSpec
  ( spec
  ) where

import           Data.Hourglass
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

import           Coach.Messaging (auditAndText)
import           Coach.Parsing   (parseDate)
import           Test.Hspec

spec :: Spec
spec = do
  let now = DateTime today (TimeOfDay 19 0 0 0)
      today = fromJust $ parseDate "11/06/2019"
      yesterday = fromJust $ parseDate "11/05/2019"
      delinquentActivity = ("Climbing", [(yesterday, "NIAD")])
      doneActivity =
        ( "Surfing"
        , [(yesterday, "OB ankle bites"), (today, "crushing with scott")])
  describe "Auditing a person" $ do
    context "When they have delinquent activities" $ do
      let people = Map.singleton "Denny" [delinquentActivity, doneActivity]
      it "sends a text" $ do
        pending
        let response = auditAndText now "Denny" people
        response `shouldBe` Just True
    context "When they are all good" $ do
      let people = Map.singleton "Denny" [doneActivity]
      it "doesn't send a text" $ do
        pending
        let response = auditAndText now "Denny" people
        response `shouldBe` Nothing
