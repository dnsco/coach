import           Control.Exception.Base (evaluate)
import           Data.Map.Strict        as Map
import           Data.Maybe             (fromJust)
import           Test.Hspec

import           Coach.Parsing

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
  where
    csvStr = trim (unlines csvRows)
    csvRows =
      [ ",,8/13/2087,8/14/2087,8/15/2087"
      , "ben,xylophon,x,,8Mile,"
      , "michaela,raging"
      , "ben,fishing,x,x,THIIIISSSS BIGGGG,"
      , ",,,,"
      ]

--    describe "New Parse" $ do
--      let csv' = fromJust . rightToMaybe $ CSV.parseCSV "test file" newCsvStr
--      let newParsed = newParse csv'
--      it
--        "has the names and labels first (with items in spreadsheet bottom-up order)" $ do
--        newParsed !! 0 `shouldBe` ["whom", "michaela", "ben"]
--        newParsed !! 1 `shouldBe` ["wat", "raging", "xylophon"]
--      it "sorts the rows reverse chronologically" $ do
--        newParsed !! 2 `shouldBe` ["8/15/2087", "", "8Mile"]
--        newParsed !! 3 `shouldBe` ["8/14/2087", "", ""]
--        newParsed !! 4 `shouldBe` ["8/13/2087", "first day out", "x"]
--    newCsvStr = trim (unlines newCSVRows)
--    newCSVRows =
--      [ "whom,wat,8/13/2087,8/14/2087,8/15/2087"
--      , "ben,xylophon,x,,8Mile,"
--      , ",,,,"
--      , "michaela,raging,first day out,,,"
--      , ",,,,"
--      ]
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

type CoachCSVParser = String -> PeopleData

parseCsv' :: CoachCSVParser
parseCsv' csvStr =
  fromJust (rightToMaybe (parseAndProcess "/test/file/fake.csv" csvStr))
