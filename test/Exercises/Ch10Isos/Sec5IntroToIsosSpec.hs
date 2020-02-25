{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch10Isos.Sec5IntroToIsosSpec where

import Test.Hspec
import Control.Lens
import Numeric.Lens
import Data.List (transpose)
import Data.Char (isUpper, toUpper, toLower)

default (Int, Double)


newtype Fahrenheit = Fahrenheit {_fahrenheit :: Double}
  deriving (Show, Eq)
  deriving newtype Num
makeLenses ''Fahrenheit
newtype Celsius = Celsius {_celcius :: Double}
  deriving (Show, Eq)
  deriving newtype Num
makeLenses ''Celsius

spec :: Spec
spec = do
  describe "1. For each of the following tasks, choose whether it’s best suited to a Lens, Traversal, Prism, or Iso" do
    it "Focus a Celsius temperature in Fahrenheit" $ 
      "Iso" `shouldBe` "Iso" -- the conversion is reversible

    it "Focus the last element of a list" $ 
      "Traversal" `shouldBe` "Traversal" -- we’re selecting a portion of a larger structure which might be missing.

    it "View a JSON object as its corresponding Haskell Record" $
      "Prism" `shouldBe` "Prism" -- We can always build valid JSON from a record, but may fail to construct a record from the json.

    it "Rotate the elements of a three-tuple one to the right" $ 
      "Iso" `shouldBe` "Iso" -- We can reverse it by rotating back to the left or rotating twice more to the right.
      
    it "Focus on the ‘bits’ of an Int as Bools." $ 
      "Traversal" `shouldSatisfy` flip elem ["Traversal", "Prism"] -- Either of Traversal' Int Bool or Prism [Bool] Int would be reasonable

    it "Focusing an IntSet from a Set Int" $ 
      "Iso" `shouldBe` "Iso" -- The conversion is trivially reversible and lossless.

  describe "2. Fill in the blank" do
    it "A" do
      let fillIn = swapped
      let result = ("Beauty", "Age") ^. fillIn
      let answer = ("Age", "Beauty")
      result `shouldBe` answer

    it "B" do
      let fillIn = from
      let result = 50 ^. fillIn (adding 10)
      let answer = 40 :: Int
      result `shouldBe` answer

    it "C" do
      let fillIn = 4 :: Double
      let result = 0 & multiplying fillIn +~ 12
      let answer = 3.0
      result `shouldBe` answer

    it "D" do
      let fillIn = 24 :: Double
      let result = 0 & adding 10 . multiplying 2 .~ fillIn
      let answer = 2
      result `shouldBe` answer

    it "E" do
      let fillIn = drop 1
      let result = [1, 2, 3] & reversed %~ fillIn
      let answer = [1, 2]
      result `shouldBe` answer

    it "F" do
      let fillIn = flipped
      let result = (view fillIn (++)) [1, 2] [3, 4]
      let answer = [3,4,1,2]
      result `shouldBe` answer

    it "G" do
      let fillIn = (^.)
      let result = [1, 2, 3] `fillIn` reversed
      let answer = [3, 2, 1]
      result `shouldBe` answer

  describe "BONUS: Hard ones ahead!" do
    -- Note: transpose flips the rows and columns of a nested list:
    it "A" do
      let fillIn = tail
      let result = [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ fillIn
      let answer = [[2,3],[20,30]]
      result `shouldBe` answer

    -- Extra hard: use `switchCase` somehow to make this statement work
    it "B" do
      let switchCase c = if isUpper c then toLower c else toUpper c
      let fillIn = involuted $ fmap switchCase
      let result = (32, "Hi") & _2 . fillIn .~ ("hELLO" :: String)
      let answer = (32,"Hello")
      result `shouldBe` answer

  it "3. You can convert from Celsius to Fahrenheit using the following formula `celsiusToF :: Double -> Double celsiusToF c = (c * (9/5)) + 32`. Implement the fc iso" do
    let fc :: Iso' Celsius Fahrenheit
        fc = celcius .
             (multiplying (9/5) . adding 32 :: Iso' Double Double) .
             from fahrenheit
    ((-40) :: Celsius) ^. fc `shouldBe` ((-40) :: Fahrenheit)
    ((-40) :: Fahrenheit) ^. from fc `shouldBe` ((-40) :: Celsius)
    (100 :: Celsius) ^. fc `shouldBe` (212 :: Fahrenheit)
    (212 :: Fahrenheit) ^. from fc `shouldBe` (100 :: Celsius)
    (0 :: Celsius) ^. fc `shouldBe` (32 :: Fahrenheit)
    (32 :: Fahrenheit) ^. from fc `shouldBe` (0 :: Celsius)

