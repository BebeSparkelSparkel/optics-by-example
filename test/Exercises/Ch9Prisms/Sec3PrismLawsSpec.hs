{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch9Prisms.Sec3PrismLawsSpec where

import Test.Hspec
import Control.Lens
import qualified Data.Set as S
import Data.Functor
import CheckPrism
import Test.QuickCheck
import Data.Char


default (Int)

spec :: Spec
spec = focus do
  describe "1. Implement the _Contains prism and determine whether it’s lawful. It should match on sets which contain the provided element! Reviewing adds the element to the set." do
    let _Contains :: forall a. Ord a => a -> Prism' (S.Set a) (S.Set a)
        _Contains x = prism' embed match
          where embed = at x .~ Just ()
                match s = s ^? ix x $> sans x s
    it "Given tests" do
      (S.fromList [1, 2, 3] ^? _Contains 2) `shouldBe` Just (S.fromList [1,3])
      (S.fromList [1, 2, 3] ^? _Contains 10) `shouldBe` Nothing
      (_Contains 10 # S.fromList [1, 2, 3]) `shouldBe` S.fromList [1,2,3,10]
      (_Contains 2 # S.fromList [1, 2, 3]) `shouldBe` S.fromList [1,2,3]

    describe "Is it lawful? Why or why not?" do
      it "Law One: Review-Preview" $ expectFailure $ property \x -> lawOneReviewPreview $ _Contains x
      -- _Contains does not pass law one if the set contains the value that is passed into _Contains because that value is removed when previewed.
      it "Law Two: Prism Complement" $ property \x -> lawTwoPrismComplement $ _Contains x
      it "Law Three: Pass-through Reversion" $ property \x -> lawThreePassThroughReversion $ _Contains x

  describe "2. Is the following prism lawful? Write out the checks to confirm your suspicions." do
    let _Singleton :: forall a. Prism' [a] a
        _Singleton = prism' embed match
          where
            embed :: a -> [a]
            embed a = [a]
            match :: [a] -> Maybe a
            match [a] = Just a
            match _ = Nothing
    it "Law One: Review-Preview" $ lawOneReviewPreview _Singleton
    it "Law Two: Prism Complement" $ lawTwoPrismComplement _Singleton
    it "Law Three: Pass-through Reversion" $ lawThreePassThroughReversion _Singleton
    -- yes, _Singleton is lawful

  describe "3. Write your own prism which fails the first law!" do
    let _MyOwnFailsFirst :: Prism' Char Char
        _MyOwnFailsFirst = prism' embed match
          where
            embed = toLower
            match c | isLower c = Just c
                    | otherwise = Nothing
    it "Law One: Review-Preview" $ expectFailure $ lawOneReviewPreview _MyOwnFailsFirst
    it "Law Two: Prism Complement" $ lawTwoPrismComplement _MyOwnFailsFirst
    it "Law Three: Pass-through Reversion" $ lawThreePassThroughReversion _MyOwnFailsFirst

