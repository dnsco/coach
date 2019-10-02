import           Coach
import           Coach.Util
import           Data.Map   as Map
import           Data.Maybe (fromJust)
import           Test.Hspec

csvRows :: [String]
csvRows =
  [ ",,8/13/2087,8/14/2087,8/15/2087"
  , "ben,xylophon,x,,8Mile"
  , "michaela,raging"
  , "ben,fishing,x,x,THIIIISSSS BIGGGG"
  ]

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
      context "Formatting people" $ do
        let people' = fromJust csvResult
            getPerson' s = fromJust (Map.lookup s people')
        it "parse people's data" $ do
          Map.member "ben" people' `shouldBe` True
          fst (head (getPerson' "ben")) `shouldBe` "fishing"
    describe "finding delinquents" $ do
      let august15 = fromJust (parseDate "08/15/2087")
          august16 = fromJust (parseDate "08/16/2087")
          findDelinquents = flip delinquents (fromJust csvResult)
      it "has people who didn't do the shit" $ do
        findDelinquents august15 `shouldBe` ["michaela"]
        findDelinquents august16 `shouldBe` ["ben", "michaela"]

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

csvStr :: String
csvStr = trim (unlines csvRows)
