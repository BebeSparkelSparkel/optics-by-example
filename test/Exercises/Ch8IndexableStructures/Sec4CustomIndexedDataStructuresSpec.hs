{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch8IndexableStructures.Sec4CustomIndexedDataStructuresSpec where

import Control.Lens
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Control.Arrow
import Data.CaseInsensitive
import Data.Map


default (Int)


newtype Cycled a = Cycled [a]
  deriving (Show, Eq)
  deriving newtype Arbitrary
type instance Index (Cycled a) = Int
type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  ix _ _ c@(Cycled []) = pure c
  ix cIndex f (Cycled cycleVals)
    | cIndex >= 0 = Cycled <$> ci cycleVals cIndex mempty mempty
    | otherwise = Cycled . reverse <$> ci (reverse cycleVals) (negate $ cIndex + 1) mempty mempty
    where ci cs i xs (y:ys)
            | i <= 0 = pure (zipStack xs . (: ys)) <*> f y
            | otherwise = ci cs (i - 1) (y:xs) ys
          ci cs i _ [] = ci cs i [] cs

zipStack :: [a] -> [a] -> [a]
zipStack (x:xs) ys = zipStack xs $ x : ys
zipStack [] ys = ys

newtype CIMap k a = CIMap (Map (CI k) a) deriving (Show, Eq)
type instance Index (CIMap k a) = k
type instance IxValue (CIMap k a) = a

instance (Ord k, FoldCase k) => Ixed (CIMap k a) where
  ix (mk -> k) f cim@(CIMap m) = case m !? k of
    Just a -> f a <&> \x -> CIMap $ insert k x m
    Nothing -> pure cim

instance (Functor f, Ord k, FoldCase k) => At (CIMap k a) where
  at (mk -> k) f (CIMap m) = CIMap <$> alterF f k m

spec :: Spec
spec = do
  describe "Cycled" do
    it "nothing if empty" $ property \i ->
      (Cycled [] ^? ix i) `shouldBe` Nothing

    it "always returns a value if not empty" $ property \(getNonEmpty >>> Cycled -> c) i ->
      (c ^? ix i) `shouldSatisfy` isJust

    describe "returns the correct value" do
      it "NonNegative" $ property \(NonEmpty xs) (NonNegative i) ->
        ((Cycled xs) ^? ix i) `shouldBe` Just (cycle xs !! i)

      it "Negative" $ property \(NonEmpty xs) (Negative i) ->
        ((Cycled xs) ^? ix i) `shouldBe` Just (cycle (reverse xs) !! (negate i - 1))

    it "book examples" do
      (Cycled ['a', 'b', 'c'] ^? ix 1) `shouldBe` Just 'b'
      (Cycled ['a', 'b', 'c'] ^? ix 3) `shouldBe` Just 'a'
      (Cycled ['a', 'b', 'c'] ^? ix 10) `shouldBe` Just 'b'
      -- Why not wrap around on negative numbers too?
      (Cycled ['a', 'b', 'c'] ^? ix (-1)) `shouldBe` Just 'c'
      -- We expect setting to work as well
      (Cycled ['a', 'b', 'c'] & ix 0 .~ '!') `shouldBe` Cycled "!bc"
      (Cycled ['a', 'b', 'c'] & ix 10 .~ '!') `shouldBe` Cycled "a!c"
      (Cycled ['a', 'b', 'c'] & ix (-1) .~ '!') `shouldBe` Cycled "ab!"

  -- fdescribe "1. Implement both Ixed and At for a newtype wrapper around a Map which makes indexing case insensitive, you can specialize to String or Text keys. Write the ix instance manually even though it has a default implementation. It’s okay to assume that user will only interact with the map via Ixed and At." do

