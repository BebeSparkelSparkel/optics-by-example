{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch7Traversals.Sec3TraversalCompositionSpec where

import Test.Hspec
import Control.Lens
import Data.Char (toUpper)
import qualified Data.Set as Set

default (Int)

spec :: Spec
spec = do
  describe "1. Short answer questions" do
    it "What type of optic do you get when you compose a traversal with a fold?" $
      "fold" `shouldBe` "fold"

    it "Which of the optics we’ve learned can act as a traversal?" $
      Set.fromList ["lens", "traversal"] `shouldBe` Set.fromList ["lens", "traversal"]

    it "Which of the optics we’ve learned can act as a fold?" $
      Set.fromList ["lens", "fold", "traversal"] `shouldBe` Set.fromList ["lens", "traversal", "fold"]

  describe "2. Fill in the blank to complete each expression" do
    it "A" do
      let d = ("Jurassic", "Park")
      let r = ("N/A", "N/A")
      (d & both .~ "N/A") `shouldBe` r

    it "B" do
      let d = ("Jurassic", "Park")
      let r = ("xxxxxxxx", "xxxx")
      (d & both . traversed .~ 'x') `shouldBe` r

    it "C" do
      let d = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
      let r = ("Mal", ["Kay", "Ina", "Jay"])
      (d & beside id traversed %~ take 3) `shouldBe` r

    it "D" do
      let d = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
      let r = ("Malcolm", ["Kaylee", "River", "Jayne"])
      (d & _2 . element 1 .~ "River") `shouldBe` r

    -- This one's tricky!
    it "E" do
      let d = ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
      let r = ["Die xxxxxxx Day", "Live xxx Let Die", "You xxxx Live Twice"]
      (d & traversed . elementOf worded 1 . traversed .~ 'x') `shouldBe` r

    -- A bit tougher now!
    it "F" do
      let d = ((1, 2), (3, 4))
      let r = ((2, 3), (4, 5))
      (d & both . both +~ 1) `shouldBe` r

    it "G" do
      let d = (1, (2, [3, 4]))
      let r = (2, (3, [4, 5]))
      (d & beside id (beside id traversed) +~ 1) `shouldBe` r

    describe "H" do
      let d = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
      let r = ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

      it "filtered" $
        (d & each . filtered fst . _2 . takingWhile (/= 'b') traversed %~ toUpper) `shouldBe` r

      it "filteredBy" $
        (d & each . filteredBy (_1 . only True) . _2 . takingWhile (/= 'b') traversed %~ toUpper) `shouldBe` r

    it "I" do
      let d = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
      let r = ("Strawberries", "Blueberries", "Blackberries")
      (d & each %~ snd) `shouldBe` r

