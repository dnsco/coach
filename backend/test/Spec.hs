import           Coach.Parsing
import           Coach.Structures
import           Data.Hourglass
import           Test.Hspec

import           Data.Map.Strict        as Map
import           Data.Set               as Set

import           Control.Exception.Base (evaluate)
import           Data.Maybe             (fromJust)
import           Data.Text              (unpack)

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
        let emptyCsvLength =
              evaluate (length (parseCsv' (unlines [",,,", ",,,,"])))
        emptyCsvLength `shouldReturn` 0
      context "Formatting people" $ do
        let people' = fromJust csvResult
            getPerson' = (Map.!) people'
        it "parse people's data" $ do
          Map.member "ben" people' `shouldBe` True
          length (getPerson' "ben") `shouldBe` (2 :: Int)
          fst (head (getPerson' "ben")) `shouldBe` "fishing"
    describe "finding delinquents" $ do
      let delinquentsOn = findDelinquents csvResult
      it "has people who didn't do the shit" $
        delinquentsOn august15 `shouldBe`
        Set.fromList [("michaela", ["raging"])]
      it "filters two people" $
        delinquentsOn august14 `shouldBe`
        Set.fromList [("michaela", ["raging"]), ("ben", ["xylophon"])]
      it "groups people's failed activities" $
        delinquentsOn august16 `shouldBe`
        Set.fromList
          [("michaela", ["raging"]), ("ben", ["fishing", "xylophon"])]
  where
    csvStr = trim (unlines csvRows)
    csvRows =
      [ ",,8/13/2087,8/14/2087,8/15/2087"
      , "ben,xylophon,x,,8Mile,"
      , "michaela,raging"
      , "ben,fishing,x,x,THIIIISSSS BIGGGG,"
      , ",,,,"
      ]
    august14 =
      DateTime {dtDate = fromJust $ parseDate "08/14/2087", dtTime = fourAm}
    august15 =
      DateTime {dtDate = fromJust $ parseDate "08/15/2087", dtTime = fourAm}
    august16 =
      DateTime {dtDate = fromJust $ parseDate "08/16/2087", dtTime = fourAm}
    fourAm = TimeOfDay {todHour = 4, todMin = 0, todSec = 0, todNSec = 0}

findDelinquents :: Maybe PeopleData -> DateTime -> Set (Person, [String])
findDelinquents csv' date =
  Set.fromList (toOldApi (delinquents date (fromJust csv')))

toOldApi :: Map Person [DActivity] -> [(Person, [String])]
toOldApi =
  Map.foldlWithKey
    (\people name acts ->
       case [unpack an | (an, isDelinquent, _) <- acts, isDelinquent] of
         []    -> people
         acts' -> people ++ [(name, acts')])
    []

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

type CoachCSVParser = String -> PeopleData

parseCsv' :: CoachCSVParser
parseCsv' csvStr =
  fromJust (rightToMaybe (parseAndProcess "/test/file/fake.csv" csvStr))
