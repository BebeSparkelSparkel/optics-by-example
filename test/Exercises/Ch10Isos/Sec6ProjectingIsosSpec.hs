{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch10Isos.Sec6ProjectingIsosSpec where

import Test.Hspec
import Control.Lens

default (Int)

spec :: Spec
spec = do
  describe "1. Fill in the blank" do
    it "A" do
      let fillIn = swapped
      let result = ("Beauty", "Age") ^. mapping reversed . fillIn
      let answer = ("egA","Beauty")
      result `shouldBe` answer

    it "B" do
      let fillIn = involuted
      let result = [True, False, True] ^. mapping (fillIn not)
      let answer = [False,True,False]
      result `shouldBe` answer

    it "C" do
      let fillIn = mapping $ involuted not
      let result = [True, False, True] & fillIn %~ filter id
      let answer = [False]
      result `shouldBe` answer

    it "D" do
      let fillIn = mapping reversed
      let result = (show ^. fillIn) 1234
      let answer = "4321"
      result `shouldBe` answer

  describe "2. Using the enum iso provided by lens `enum :: Enum a => Iso' Int a`." do
    it "Implement the following intNot function, use dimapping in your implementation." do
      let intNot :: Int -> Int
          intNot = not ^. dimapping enum (from enum)
      intNot 0 `shouldBe` 1
      intNot 1 `shouldBe` 0
      -- shouldThrow (pure $ intNot 2) anyErrorCall

    it "Can you simplify your implementation of this function somehow?" do
      let intNot :: Int -> Int
          intNot = enum %~ not
      intNot 0 `shouldBe` 1
      intNot 1 `shouldBe` 0
      -- shouldThrow (pure $ intNot 2) anyErrorCall
