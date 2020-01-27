{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch6Folds.Sec3FoldActionsSpec where

import Test.Hspec
import Control.Lens
import Control.Applicative
import Data.Function

default (Int)


spec :: Spec
spec = do
  -- elemOf :: Eq a => Fold s a -> a -> s -> Bool
  -- has :: Fold s a -> s -> Bool
  -- lengthOf :: Fold s a -> s -> Int
  -- sumOf :: Numn => Fold s n -> s -> n
  -- productOf :: Num n => Fold s n -> s -> n
  -- foldOf :: Monoid a => Fold s a -> s -> a
  -- preview :: Fold s a -> s -> Maybe a
  -- lastOf :: Fold s a -> s -> Maybe a
  -- minimumOf :: Ord a => Fold s a -> s -> Maybe a
  -- maximumOf :: Ord a => Fold s a -> s -> Maybe a
  -- anyOf :: Fold s a -> (a -> Bool) -> s -> Bool
  -- allOf :: Fold s a -> (a -> Bool) -> s -> Bool
  -- findOf :: Fold s a -> (a -> Bool) -> s -> Maybe a
  -- foldrOf :: Fold s a -> (a -> r -> r) -> r -> s -> r
  -- foldMapOf :: Monoid r => Fold s a -> (a -> r) -> s -> r
  describe "1. Pick the matching action from the list for each example:" do
    it "_ folded []" $
      (has folded []) `shouldBe` False

    it "_ both (\"Yo\", \"Adrian!\")" $
      (foldOf both ("Yo", "Adrian!")) `shouldBe` "YoAdrian!"

    it "_ each \"phone\" (\"E.T.\", \"phone\", \"home\")" $
      (elemOf each "phone" ("E.T.", "phone", "home")) `shouldBe` True

    it "_ folded [5, 7, 2, 3, 13, 17, 11]" $
      (minimumOf folded [5, 7, 2, 3, 13, 17, 11]) `shouldBe` Just 2

    it "_ folded [5, 7, 2, 3, 13, 17, 11]" $
      (lastOf folded [5, 7, 2, 3, 13, 17, 11]) `shouldBe` Just 11

    it "_ folded ((> 9) . length) [\"Bulbasaur\", \"Charmander\", \"Squirtle\"]" $
      (anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]) `shouldBe` True

    it "_ folded even [11, 22, 3, 5, 6]" $
      (findOf folded even [11, 22, 3, 5, 6]) `shouldBe` Just 22

  describe "2. Use an action from the list along with any fold you can devise to retrieve the output from the input in each of the following challenges. There may be more than one correct answer." do
    it "Find the first word in the input list which is a palindrome; I.e. a word that’s the same backwards as forwards." do
      let input = ["umbrella", "olives", "racecar", "hammer"]
      let output = Just "racecar"
      findOf folded (liftA2 (==) id reverse) input `shouldBe` output

    it "Determine whether all elements of the following tuple are EVEN" do
      let input = (2, 4, 6)
      let output = True
      allOf each even input `shouldBe` output

    it "Find the pair with the largest integer" do
      let input = [("I'll", 2), ("Be", 3), ("Back", 1)]
      let output = Just ("Be", 3)
      maximumByOf folded (compare `on` snd) input `shouldBe` output

    it "Find the sum of both elements of a tuple. This one may require additional alterations AFTER running a fold." do
      let input = (1, 2)
      let output = 3
      sumOf each input `shouldBe` output

    describe "3. BONUS – These are a bit trickier" do
      -- it "Find which word in a string has the most vowels." do
      --   let input = "Do or do not, there is no try."
      --   let output = Just "there"
      --   -- findOf (to words) (const True) input `shouldBe` output

      it "Combine the elements into the expected String" do
        let input = ["a", "b", "c"]
        let output = "cba"
        foldrOf folded (flip (<>)) mempty input `shouldBe` output

      describe "Good luck with the following ones! Get it to work however you can!" do
        it "A" do
          let input = [(12, 45, 66), (91, 123, 87)]
          let output = "54321"
          foldMapOf (folded . _2) (reverse . show) input `shouldBe` output

        it "B" do
          let input = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
          let output = ["b", "d"]
          let f (n, s) | even n = [s]
                       | otherwise = []
          foldMapOf folded f input `shouldBe` output

