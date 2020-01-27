{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch6Folds.Sec4HigherOrderFoldsSpec where

import Test.Hspec
import Control.Lens
import Data.Char (isAlpha)

default (Int)


spec :: Spec
spec = do
  describe "1. Fill in the blank. You’ll need to remember some tricks from previous sections!" do
    it "A" $ ("Here's looking at you, kid" ^.. dropping 7 folded)
      `shouldBe` "looking at you, kid"

    it "B" $ (["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 worded)
      `shouldBe` ["My","Hakuna","No"]

    it "C" $
      ( ["My Precious", "Hakuna Matata", "No problemo"] ^..
        taking 1 (folded . worded)
      ) `shouldBe` ["My"]

    it "D" $
      ( ["My Precious", "Hakuna Matata", "No problemo"] ^..
        folded . taking 1 worded . folded
      ) `shouldBe` "MyHakunaNo"

    it "F" $
      ( ["My Precious", "Hakuna Matata", "No problemo"] ^..
        folded . takingWhile isAlpha folded
      ) `shouldBe` "MyHakunaNo"

    it "G" $ (sumOf (taking 2 each) (10, 50, 100)) `shouldBe` 60

    it "H" $ (("stressed", "guns", "evil") ^.. backwards each)
      `shouldBe` ["evil","guns","stressed"]

    it "I" $ (("stressed", "guns", "evil") ^.. backwards each . to reverse)
      `shouldBe` ["live","snug","desserts"]

    it "K" $ ("blink182 k9 blazeit420" ^.. worded . droppingWhile isAlpha folded)
      `shouldBe` "1829420"

  describe "2. Solve the following problems using higher-order folds" do
    -- We’re doing a temperature study in a given region, but we need to run tests on several subsets of the data.
    -- Here’s the series of daily temperature measurements we’ve been given for the region:
    let sample :: [Int]
        sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7, -6]
    it "First we’re interested in how many days it took until the first thaw. Write an expression which calculates the number of measurements until a temp is above zero." $
      (lengthOf (takingWhile (<= 0) folded) sample) `shouldBe` 2

    it "For the next study we need to know the warmest it got in the first 4 days of the sample. Write an expression to calculate it." $
      (maximumOf (taking 4 folded) sample) `shouldBe` Just 4

    it "Write an expression to calculate the temperature on the day AFTER we hit that temperature. Use preview or ^? somewhere in that expression if you can." $
      (sample ^? dropping 3 folded) `shouldBe` Just 3

    it "How many days of below-freezing weather did we have consecutively at the END of the sample?" $
      (lengthOf (takingWhile (< 0) (backwards folded)) sample) `shouldBe` 3

    it "Now we’re interested in running statistics on temperature data specifically from the first thaw until the next freeze. Write an expression which lists out all temperature samples from the first time we sample above 0, until the next time we’re below zero." $
      -- (sample ^.. (droppingWhile (<= 0) $ takingWhile (> 0) $ folded)) `shouldBe` [4, 3, 8, 6]
      (sample ^.. (takingWhile (> 0) $ droppingWhile (<= 0) $ folded)) `shouldBe` [4, 3, 8, 6]

    describe "BONUS: List out all the temperature samples between the FIRST thaw and the FINAL freeze." do
      it "Generalize this behaviour into a function: trimmingWhile. It should drop elements from the start AND end of a fold while a predicate is True." do
        let trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
            trimmingWhile p f = backwards $ droppingWhile p $ backwards $ droppingWhile p f
        (sample ^.. trimmingWhile (<= 0) folded) `shouldBe` [4, 3, 8, 6, -2, 3]

