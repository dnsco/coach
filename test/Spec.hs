import           Data.Char  (isSpace)
import           Data.Map   as Map
import           Data.Maybe (fromJust)
import           Lib
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    let csvResult = rightToMaybe (parseAndProcess "/test/file/fake.csv" csv)
    describe "Paring a csv" $ do
      it "seperates the people from the header" $ do
        let csvRowCount = length rows
            mapEntryCount = Map.size <$> csvResult
        ((csvRowCount >) <$> mapEntryCount) `shouldBe` Just True
        mapEntryCount `shouldBe` (Just 2 :: Maybe Int)
      context "Formatting people" $ do
        let people' = fromJust csvResult
            getPerson' s = fromJust (Map.lookup s people')
        it "parse people's data" $ do
          Map.member "ben" people' `shouldBe` True
          fst (head (getPerson' "ben")) `shouldBe` "fishing"
  where
    csv = trim (unlines rows)
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
