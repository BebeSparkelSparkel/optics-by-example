{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch9Prisms.Sec1IntroductionToPrismsSpec where

import Test.Hspec
import Control.Lens


data ContactInfo
  = Email String
  | Telephone Int
  | Address String String String
  deriving (Show, Eq)
makePrisms ''ContactInfo

spec :: Spec
spec = do
  describe "1. Which prisms will be generated from the following data declaration? Give their names and types." do
    it "_Email" do
      (_Email # "Hi") `shouldBe` Email "Hi"
      (Email "HI" ^? _Email) `shouldBe` Just "HI"
      (Telephone 1 ^? _Email) `shouldBe` Nothing

    it "_Telephone" do
      (_Telephone # 55) `shouldBe` Telephone 55
      (Telephone 23 ^? _Telephone) `shouldBe` Just 23
      (Email "NONe" ^? _Telephone) `shouldBe` Nothing

    it "_Address" do
      (_Address # ("a","b","c")) `shouldBe` Address "a" "b" "c"
      (Address "32" "Rd" "NV" ^? _Address) `shouldBe` Just ("32","Rd","NV")
      (Email "Not address" ^? _Address) `shouldBe` Nothing

  describe "2. Fill in the blanks" do
    it "A" do
      let fillIn = _Right
      let result = Right 35 & fillIn +~ 5
      let answer = Right 40
      shouldBe result answer

    it "B" do
      let fillIn = _Just
      let result = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
            ^.. folded . fillIn
      let answer = ["Mind","Power","Soul","Time"]
      shouldBe result answer

    it "C" do
      let fillIn = traversed . _Just
      let result = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] &
            fillIn <>~ " Stone"
      let answer = [ Just "Mind Stone"
            , Just "Power Stone"
            , Nothing
            , Just "Soul Stone"
            , Nothing
            , Just "Time Stone"
            ]
      shouldBe result answer

    it "D" do
      let fillIn = _Left . _1 . _Right
      let result = Left (Right True, "Eureka!") & fillIn %~ not
      let answer = Left (Right False, "Eureka!")
      shouldBe result answer

    it "E" do
      let fillIn = review
      let result = _Cons `fillIn` ("Do",["Re", "Mi"])
      let answer = ["Do", "Re", "Mi"]
      shouldBe result answer

    it "F" do
      let result = isn't (_Show :: Prism' String Int) "not an int"
      let answer = True
      shouldBe result answer

  describe "3. Write an expression to get the output from the provided input." do
    it "A" do
      let input = (Just 1, Nothing, Just 3)
      let result = input ^.. each . _Just
      let output = [1, 3]
      result `shouldBe` output

    it "B" do
      let input = ('x', "yz")
      let result = input & _2 %~ reverse & review _Cons
      let output = "xzy"
      result `shouldBe` output

    it "C" do
      let input = "do the hokey pokey"
      let result = _Left . _Just . _Right # input
      let output = Left (Just (Right "do the hokey pokey"))
      result `shouldBe` output

