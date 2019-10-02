import           Data.Char    (isSpace)
import           Data.Functor ((<&>))
import           Data.Map     as Map
import           Data.Maybe   (fromJust)
import           Lib
import           Test.Hspec
import           Text.CSV     as CSV (parseCSV)

--  let parsed = processRows <$> fakeData)
--      ben = parsed <&> Map.lookup "ben"
--   in
main :: IO ()
main =
  hspec $ do
    describe "Paring a csv" $ do
      let csv = trim (unlines rows)
          csvData = rightToMaybe (CSV.parseCSV "www.anunittest.com" csv)
          processed = rightToMaybe (parseAndProcess "/test/file/fake.csv" csv)
      it "seperates the people from the header" $ do
        let csvRowCount = csvData <&> length
            mapEntryCount = Map.size <$> processed
        (>) <$> csvRowCount <*> mapEntryCount `shouldBe` Just True
        mapEntryCount `shouldBe` (Just 2 :: Maybe Int)
      context "Formatting people" $ do
        let people' = fromJust processed
            getPerson' s = fromJust (Map.lookup s people')
        it "parse people's data" $ do
          Map.member "ben" people' `shouldBe` True
          fst (head (getPerson' "ben")) `shouldBe` "fishing"
  where
    rows =
      [ ",,8/13/2087,8/14/2087,8/15/2087"
      , "ben,xylophon,x,,8Mile"
      , "michaela,raging"
      , "ben,fishing,x,x,THIIIISSSS BIGGGG"
      ]

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

trim :: String -> String
trim = (\f -> f . f) (reverse . dropWhile isSpace)
