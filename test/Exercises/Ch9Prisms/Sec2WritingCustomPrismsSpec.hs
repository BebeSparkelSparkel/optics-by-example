module Exercises.Ch9Prisms.Sec2WritingCustomPrismsSpec where

import Test.Hspec
import Control.Lens
import Test.QuickCheck


_Factor :: Int -> Prism' Int Int
_Factor x = prism' embed match
  where
    embed = (* x)
    match y = if y `mod` x == 0
      then Just y
      else Nothing

fizz :: Prism' String Int
fizz = prism' embed match
  where
    embed x = if x `mod` 3 == 0
      then "fizz"
      else ""
    match "fizz" = Just 3
    match _ = Nothing

buzz :: Prism' String Int
buzz = prism' embed match
  where
    embed x = if x `mod` 5 == 0
      then "buzz"
      else ""
    match "buzz" = Just 5
    match _ = Nothing

spec :: Spec
spec = focus do
  describe "_Factor" do
    it "review" $ property \f i -> (_Factor f # i) `shouldBe` (f * i)
    it "is match" $ property \(Positive f) i -> has (_Factor f) (f * i)

  describe "fizzbuzz" do
    describe "fizz" do
      it "review" do
        (fizz # 3) `shouldBe` "fizz"
        (fizz # 4) `shouldBe` ""

      it "match" do
        ("fizz" ^? fizz) `shouldBe` Just 3
        ("buzz" ^? fizz) `shouldBe` Nothing

    describe "buzz" do
      it "review" do
        (buzz # 5) `shouldBe` "buzz"
        (buzz # 4) `shouldBe` ""

      it "match" do
        ("buzz" ^? buzz) `shouldBe` Just 5
        ("fizz" ^? buzz) `shouldBe` Nothing

