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
    describe "FetchAndParse" $ do
      it "seperates the people from the header" $ do
        (csvData <&> length) `shouldBe` (Just 2 :: Maybe Int)
        Map.size <$> processed `shouldBe` (Just 1 :: Maybe Int)
      it "parse people's data" $ do
        Map.member "ben" people' `shouldBe` True
        fst (head (getPerson' "ben")) `shouldBe` "fishing"
  where
    rows =
      [",,8/13/2087,8/14/2087,8/15/2087", "ben,fishing,x,x,THIIIISSSS BIGGGG"]
    csv' = trim (unlines rows)
    csvData = rightToMaybe (CSV.parseCSV "www.anunittest.com" csv')
    processed = processRows <$> csvData
    people' = fromJust processed
    getPerson' s = fromJust (Map.lookup s people')

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

trim :: String -> String
trim = (\f -> f . f) (reverse . dropWhile isSpace)
