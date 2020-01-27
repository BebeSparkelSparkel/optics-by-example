{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch6Folds.Sec1IntroductionToFoldsSpec where

import Test.Hspec
import Control.Lens
import Data.Map as M
import Data.Sequence as S
import Data.Text as T

default (String, Int)


spec :: Spec
spec = do
  describe "1. What’s the result of each expression? Make sure to guess before trying it out in the repl!" do
    let beastSizes :: [(Int, String)]
        beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]
    it "A" $ (beastSizes ^.. folded) `shouldBe` beastSizes
    it "B" $ (beastSizes ^.. folded . folded) `shouldBe` snd <$> beastSizes
    it "C" $ (beastSizes ^.. folded . folded . folded) `shouldBe` foldMap snd beastSizes
    it "D" $ (beastSizes ^.. folded . _2) `shouldBe` snd <$> beastSizes
    it "E" $ (toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]) `shouldBe` [1..6]
    it "F" $ ( toListOf
                (folded . folded)
                (M.fromList [("Jack", "Captain" :: String), ("Will", "First Mate")]) )
                `shouldBe` "CaptainFirst Mate"
    it "G" $ (("Hello" :: String, "It's me") ^.. both . folded) `shouldBe` "HelloIt's me"
    it "H" $ (("Why", "So", "Serious?") ^.. each) `shouldBe` ["Why", "So", "Serious?"]
    let quotes :: [(T.Text, T.Text, T.Text)]
        quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]
    it "I" $ (quotes ^.. each . each . each) `shouldBe` "WhySoSerious?ThisisSPARTA"

  describe "2. Write out the ‘specialized’ type for each of the requested combinators used in each of the following expressions." do
    it "folded and _1" $ toListOf
      ( (folded :: Fold [(Int, Char)] (Int, Char)) .
        (_1 :: Fold (Int, Char) Int)
      )
      [(1, 'a'), (2, 'b'), (3, 'c')]
      `shouldBe` [1, 2, 3]

    it "folded, _2, and toListOf" $ toListOf
      ( (_2 :: Fold (Bool, Seq String) (Seq String)) .
        (folded :: Fold (Seq String) String)
      )
      (False, S.fromList ["one", "two", "three"])
      `shouldBe` ["one", "two", "three"]

    it "folded and folded and toListOf" $ toListOf
      ( (folded :: Fold (Map String String) String) .
        (folded :: Fold String Char)
      )
      (M.fromList [("Jack", "Captain" :: String), ("Will", "First Mate")])
      `shouldBe` "CaptainFirst Mate"
