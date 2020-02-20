{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch7Traversals.Sec7AdvancedManipulation where

import Test.Hspec
import Control.Lens
import qualified Data.Map as M

default (Int)


spec :: Spec
spec = do
  describe "What fits in the blanks?" do
    it "A" do
      let answer = [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
      let fillIn = [2, 4]
      fillIn `shouldBe` answer

    it "B" do
      let answer = ["Aardvark", "Bandicoot", "Capybara"]
            ^. traversed . partsOf (taking 3 traversed)
      let fillIn = "AarBanCop"
      fillIn `shouldBe` answer

    it "C" do
      let answer = [1,2,3,4]
      let fillIn = beside traversed traversed
      let result = ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf fillIn
      result `shouldBe` answer

    it "D" do
      let answer = [1,20,3,40]
      let fillIn = filtered even
      let result = [1, 2, 3, 4] & partsOf (traversed . fillIn) .~ [20, 40]
      result `shouldBe` answer

    it "E" do
      let answer = ["Kangaroo","Bandicoot","Capybara"]
      let fillIn = traversed . traversed
      let result = ["Aardvark", "Bandicoot", "Capybara"] & partsOf fillIn .~ "Kangaroo"
      result `shouldBe` answer

    it "F" do
      let answer = ["Aardvark", "Bandicoot", "Capybara"]
            & partsOf (traversed . traversed) .~ "Ant"
      let fillIn = ["Antdvark", "Bandicoot", "Capybara"]
      fillIn `shouldBe` answer

    it "G" do
      -- Modifying
      -- Tip: Map values are traversed in order by KEY
      let answer = M.fromList [('a','b'),('b','c'),('c','a')]
      let fillIn [] = []
          fillIn (x:xs) = xs <> [x]
      let result = M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')]
            & partsOf traversed %~ fillIn
      result `shouldBe` answer

    it "H" do
      let answer = ('c', 'b', 'a')
      let fillIn = partsOf each
      let result = ('a', 'b', 'c') & fillIn %~ reverse
      shouldBe result answer

    it "Bonus I" do
      let answer = [3,2,1,4,5,6]
      let fillIn = taking 3 traversed
      let result = [1, 2, 3, 4, 5, 6] & partsOf fillIn %~ reverse
      shouldBe result answer

    it "Bonus J" do
      let answer = (("abc",'a'),("abc",'b'),("abc",'c'))
      let fillIn = unsafePartsOf
      let result = ('a', 'b', 'c') & fillIn each %~ \xs -> fmap ((,) xs) xs
      shouldBe result answer
