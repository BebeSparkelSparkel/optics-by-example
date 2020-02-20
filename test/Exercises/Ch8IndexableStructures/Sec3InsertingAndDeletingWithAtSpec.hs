
module Exercises.Ch8IndexableStructures.Sec3InsertingAndDeletingWithAtSpec where

import Test.Hspec
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

default (Int)


spec :: Spec
spec = do
  describe "1. Fill in the Blanks" do
    it "A" do
      let answer = ["Larry","Wiggly","Moe"]
      let fillIn = ix
      let result = ["Larry", "Curly", "Moe"] & fillIn 1 .~ "Wiggly"
      shouldBe result answer

    let heroesAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]

    it "B" do
      let answer = M.fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex")]
      let fillIn = at
      let result = heroesAndVillains & fillIn "Spiderman" .~ Just "Goblin"
      shouldBe result answer

    it "C" do
      let answer = M.fromList [("Batman","Joker")]
      let fillIn = sans
      let result = fillIn "Superman" heroesAndVillains
      shouldBe result answer

    it "D" do
      let answer = S.fromList "aeouy"
      let fillIn1 ai v = ai .~ Just v
      let fillIn2 = Nothing
      let result = S.fromList ['a', 'e', 'i', 'o', 'u']
            & at 'y' `fillIn1` ()
            & at 'i' .~ fillIn2
      shouldBe result answer

  it "2. Use ix and at to go from the input to the output" do
    let input :: M.Map String Int
        input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
    let output = M.fromList [("candy bars",13),("ice cream",5),("soda",37)]
    let result = input
          & at "ice cream" .~ Just 5
          & ix "soda" +~ 3
          & sans "gum"
    shouldBe result output

