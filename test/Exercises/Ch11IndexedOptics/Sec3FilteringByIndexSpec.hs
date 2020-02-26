{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch11IndexedOptics.Sec3FilteringByIndexSpec where

import Control.Lens
import Test.Hspec
import qualified Data.Map as M

default (Int)


spec :: Spec
spec = focus do
  describe "1. Given the following exercise schedule:" do
    let exercises :: M.Map String (M.Map String Int)
        exercises = M.fromList
          [ ("Monday" , M.fromList [("pushups", 10), ("crunches", 20)])
          , ("Wednesday", M.fromList [("pushups", 15), ("handstands", 3)])
          , ("Friday" , M.fromList [("crunches", 25), ("handstands", 5)])
          ]
    
    it "Compute the total number of “crunches” you should do this week." do
      let result = sumOf (traversed . itraversed . index "crunches") exercises
      result `shouldBe` 45

    it "Compute the number of reps you need to do across all exercise types on Wednesday." do
      let result = sumOf (itraversed . index "Wednesday" <. itraversed) exercises
      result `shouldBe` 18

    it "List out the number of pushups you need to do each day, you can use ix to help this time if you wish." do
      let result = exercises ^@.. itraversed <. at "pushups" . non 0
      result `shouldBe` [("Friday",0),("Monday",10),("Wednesday",15)]

  describe "2. Given the following board" do
    let board = [ "XOO"
                , ".XO"
                , "X.." ]
    it "Generate a list of positions alongside their (row, column) coordinates." do
      let result = board ^@.. itraversed <.> itraversed
      result `shouldBe` [((0,0),'X'),((0,1),'O'),((0,2),'O'),((1,0),'.'),((1,1),'X'),((1,2),'O'),((2,0),'X'),((2,1),'.'),((2,2),'.')]

    it "Set the empty square at (1,0) to an 'X'. HINT: When using the custom composition operators you’ll often need to introduce parenthesis to get the right precedence." do
      -- let result = board & ix 1 . ix 0 .~ 'X'
      let result = board & (itraversed <.> itraversed) . index (1,0) .~ 'X'
      result `shouldBe`
        [ "XOO"
        , "XXO"
        , "X.." ]
      
    it "Get the 2nd column as a list (e.g. \"OX.\"). Try to do it using index instead of indices!" do
      let result = board ^.. itraversed .> itraversed . index 1
      result `shouldBe` "OX."

    it "Get the 3rd row as a list (e.g. \"X..\"). Try to do it using index instead of indices! HINT: The precedence for this one can be tricky too." do
      let result = board ^.. (itraversed <. itraversed) . index 2
      result `shouldBe` "X.."

