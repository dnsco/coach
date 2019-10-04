import           Coach
import           Coach.Util
import           Control.Exception.Base (evaluate)
import           Data.Hourglass
import           Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    let csvResult = rightToMaybe (parseAndProcess "/test/file/fake.csv" csvStr)
    describe "Paring a csv" $ do
      it "seperates the people from the header" $ do
        let csvRowCount = length csvRows
            mapEntryCount = Map.size <$> csvResult
        ((csvRowCount >) <$> mapEntryCount) `shouldBe` Just True
        mapEntryCount `shouldBe` (Just 2 :: Maybe Int)
      it "Handles Blank Rows" $ do
        let emptyCsvLength = evaluate (length (parseCsv' (unlines [",,,", ",,,,"])))
        emptyCsvLength `shouldReturn` 0
      context "Formatting people" $ do
        let people' = fromJust csvResult
            getPerson' = (Map.!) people'
        it "parse people's data" $ do
          Map.member "ben" people' `shouldBe` True
          length (getPerson' "ben") `shouldBe` (2 :: Int)
          fst (head (getPerson' "ben")) `shouldBe` "fishing"
    describe "finding delinquents" $ do
      let findDelinquents = flip delinquents (fromJust csvResult)
      it "has people who didn't do the shit" $ do
        let ds = findDelinquents august15
        ds `shouldBe` [("michaela", ["raging"])]
      it "filters two people" $ do
        let ds = findDelinquents august14
        ds `shouldBe` [("michaela", ["raging"]), ("ben", ["xylophon"])]
      it "groups people's failed activities" $ do
        let ds = findDelinquents august16
        ds `shouldBe` [("michaela", ["raging"]), ("ben", ["fishing", "xylophon"])]
  where
    csvStr = trim (unlines csvRows)
    csvRows =
      [ ",,8/13/2087,8/14/2087,8/15/2087"
      , "ben,xylophon,x,,8Mile,"
      , "michaela,raging"
      , "ben,fishing,x,x,THIIIISSSS BIGGGG,"
      , ",,,,"
      ]
    august14 = DateTime {dtDate = fromJust $ parseDate "08/14/2087", dtTime = fourAm}
    august15 = DateTime {dtDate = fromJust $ parseDate "08/15/2087", dtTime = fourAm}
    august16 = DateTime {dtDate = fromJust $ parseDate "08/16/2087", dtTime = fourAm}
    fourAm = TimeOfDay {todHour = 4, todMin = 0, todSec = 0, todNSec = 0}

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

type CoachCSVParser = String -> PeopleData

parseCsv' :: CoachCSVParser
parseCsv' csvStr = fromJust (rightToMaybe (parseAndProcess "/test/file/fake.csv" csvStr))
