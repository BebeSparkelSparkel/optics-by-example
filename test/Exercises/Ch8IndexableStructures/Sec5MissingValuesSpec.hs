{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch8IndexableStructures.Sec5MissingValuesSpec where

import Test.Hspec
import Control.Lens
import Data.Map as M

default (Int)


spec :: Spec
spec = focus do
  describe "1. Write an optic which focuses the value at key “first” or, failing that, the value at key “second”." do
    let optic = ix "first" `failing` ix "second"
    it "A" do
      let result = M.fromList [("first", False), ("second", False)]
            & optic .~ True
      let answer = M.fromList [("first",True),("second",False)]
      shouldBe result answer

    it "B" do
      let result = M.fromList [("second", False)] & optic .~ True
      let answer = fromList [("second",True)]
      shouldBe result answer

  describe "2. Write an optic which focuses the first element of a tuple if it is even, and the second tuple element otherwise. Assume each slot contains an integer." do
    let optic = _1 . filtered even `failing` _2

    it "A" do
      let result = (1, 1) & optic *~ 10
      let answer = (1,10)
      shouldBe result answer

    it "B" do
      let result = (2, 2) & optic *~ 10
      let answer = (20,2)
      shouldBe result answer

  describe "3. Write an optic which focuses all even numbers in a list, if none of the members are odd then focus ALL numbers in the list." do
    let optic = folded . filtered even `failing` folded

    it "A" do
      let result = [1, 2, 3, 4] ^.. optic
      let answer = [2,4]
      shouldBe result answer

    it "B" do
      let result = [1, 3, 5] ^.. optic
      let answer = [1,3,5]
      shouldBe result answer

  describe "4. Fill in the blanks" do
    it "A" do
      let fillIn = "default"
      let answer = Nothing ^. non "default"
      shouldBe fillIn answer

    it "B" do
      let fillIn = non 100
      let result = Nothing & fillIn +~ 7
      let answer = Just 107
      shouldBe result answer

    it "C" do
      let fillIn = non False
      let result = M.fromList [("Perogies", True), ("Pizza", True), ("Pilsners", True)] ^.
            at "Broccoli" . fillIn
      let answer = False
      shouldBe result answer

    it "D" do
      let fillIn = at "Wario's Woods" . non 0
      let result = M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)] &
            fillIn +~ 999
      let answer = M.fromList
            [ ("Breath of the wild",22000000)
            , ("Odyssey",9070000)
            , ("Wario's Woods",999) ]
      shouldBe result answer

    it "E" do
      let fillIn = pre $ ix 4
      let result = ["Math", "Science", "Geography"] ^. fillIn . non "Unscheduled"
      let answer = "Unscheduled"
      shouldBe result answer

    it "BONUS: Use 'pre' and 'non'" do
      let fillIn = folded . pre (filtered even) . non (-1)
      let result = [1, 2, 3, 4] ^.. fillIn
      let answer = [-1, 2, -1, 4]
      shouldBe result answer
