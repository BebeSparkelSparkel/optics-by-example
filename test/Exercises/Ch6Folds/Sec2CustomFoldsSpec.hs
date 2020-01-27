{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch6Folds.Sec2CustomFoldsSpec where

import Test.Hspec
import Control.Lens
import qualified Data.Sequence as S

default (Int)


spec :: Spec
spec = do
  describe "1. Fill in each blank with either to, folded, or folding." do
    it "[\"Yer\", \"a\", \"wizard\", \"Harry\"] ^.. folded . _" do
      let d :: [String]
          d = ["Yer", "a", "wizard", "Harry"]
      (d ^.. folded . folded) `shouldBe` "YerawizardHarry"

    it "[[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)" $
     ( [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)) `shouldBe` [1, 2, 4, 5]

    it "[[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)" $
      ([[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)) `shouldBe` [[1,2], [4,5]]

    it "[\"bob\", \"otto\", \"hannah\"] ^.. folded . _ reverse" $
      (["bob", "otto", "hannah"] ^.. folded . to reverse) `shouldBe` ["bob", "otto", "hannah"]

    it "(\"abc\", \"def\") ^.. _ (\\(a, b) -> [a, b]). _ reverse . _" $
      (("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded) `shouldBe` "cbafed"

  describe "2. Fill in the blank for each of the following expressions with a path of folds which results in the specified answer. Avoid partial functions and fmap." do

    it "[1..5] ^.. _" $
      ([1..5] ^.. folded . to (* 100)) `shouldBe` [100,200,300,400,500]

    it "(1, 2) ^.. _" $
      ((1, 2) ^.. both) `shouldBe` [1, 2]

    it "[(1, \"one\"), (2, \"two\")] ^.. _" $
      ([(1, "one"), (2, "two")] ^.. folded . _2) `shouldBe` ["one", "two"]

    it "(Just 1, Just 2, Just 3) ^.. _" $
      ((Just 1, Just 2, Just 3) ^.. each . _Just) `shouldBe` [1, 2, 3]

    it "[Left 1, Right 2, Left 3] ^.. _" $
      ([Left 1, Right 2, Left 3] ^.. folded . _Right) `shouldBe` [2]

    it "[([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. _" $
      ([([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . each . folded) `shouldBe`
        [1, 2, 3, 4, 5, 6, 7, 8]

    it "[1, 2, 3, 4] ^.. _" do
      let l :: Fold [Int] (Either Int Int)
          l = folded . to \x -> if odd x then Left x else Right x
      ([1, 2, 3, 4] ^.. l) `shouldBe` [Left 1, Right 2, Left 3, Right 4]

    it "[(1, (2, 3)), (4, (5, 6))] ^.. _" do
      let l = folded . folding \s ->
            s ^.. _1 <>
            s ^.. _2 . each
      ([(1, (2, 3)), (4, (5, 6))] ^.. l) `shouldBe` [1, 2, 3, 4, 5, 6]

    it "[(Just 1, Left \"one\"), (Nothing, Right 2)] ^.. _" do
      let l = folded . folding \t ->
            t ^.. _1 . _Just <>
            t ^.. _2 . _Right
      ([(Just 1, Left "one"), (Nothing, Right 2)] ^.. l) `shouldBe` [1, 2]

    it "[(1, \"one\"), (2, \"two\")] ^.. _" do
      let l = folded . folding \t ->
            t ^.. _1 . to Left <>
            t ^.. _2 . to Right
      ([(1, "one"), (2, "two")] ^.. l) `shouldBe` [Left 1, Right "one", Left 2, Right "two"]

    it "S.fromList [\"apricots\", \"apples\"] ^.. _" do
      let l :: Fold (S.Seq String) Char
          l = to S.reverse . folded . to reverse . folded
      (S.fromList ["apricots" :: String, "apples"] ^.. l) `shouldBe`
        "selppastocirpa"

  describe "3. BONUS – Devise a fold which returns the expected results. Think outside the box a bit." do
    it "[(12, 45, 66), (91, 123, 87)] ^.. _" do
      let l :: Fold [(Int,Int,Int)] Char
          l = folded . _2 . folding (reverse . show)
      ([(12, 45, 66), (91, 123, 87)] ^.. l) `shouldBe` "54321"

    it "[(1, \"a\"), (2, \"b\"), (3, \"c\"), (4, \"d\")] ^.. _" do
      let l :: Fold [(Int, String)] String
          l = folded . folding \case
            (x, c) | even x -> [c]
                   | otherwise -> []
      ([(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. l) `shouldBe` ["b", "d"]
