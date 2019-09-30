{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Arrow (right)
import           Lib
import           Test.Hspec
import           Text.CSV

fakeData = [["1"], ["2"], ["3"]]

main =
  hspec $ do
    describe "FetchAndParse" $ do
      it "is lit " $
        ((length . head) <$> processRows fakeParser fakeData) `shouldBe`
        (Right 3 :: Either _ Int)
      it "is w lit " $
        ((head . head) <$> processRows fakeParser fakeData) `shouldBe`
        (Right ["1"] :: Either _ [String])
      it "has fun" $ 2 + 2 `shouldBe` (4 :: Int)

fakeParser :: CSV -> CSVResult
fakeParser = Right
