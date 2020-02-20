module Exercises.Ch3Lenses.Sec7DataCorrectionAndMaintainingInvariantsSpec where

import CheckLens
import Control.Lens
import Barbies
import Data.Kind (Constraint)
import GHC.Generics
import Test.Hspec
import Test.QuickCheck


data ProducePrices (f :: * -> *) = ProducePrices
  { _limePrice :: f Float
  , _lemonPrice :: f Float
  } deriving (Generic, FunctorB)
deriving instance QCC Show f => Show (ProducePrices f)
deriving instance QCC Eq f => Eq (ProducePrices f)

gtzLensMake :: Wrapper f
            => (ProducePrices f -> f Float)
            -> (ProducePrices f -> f Float -> ProducePrices f)
            -> Lens' (ProducePrices f) Float
gtzLensMake viewer setter = lens
  (roundUpToZero . unwrap . viewer)
  (\s -> setter s . wrap . roundUpToZero)

limePrice :: Wrapper f => Lens' (ProducePrices f) Float
limePrice = gtzLensMake _limePrice \s x -> s {_limePrice = x}

lemonPrice :: Wrapper f => Lens' (ProducePrices f) Float
lemonPrice = gtzLensMake _lemonPrice \s x -> s {_lemonPrice = x}

instance Arbitrary (f Float) => Arbitrary (ProducePrices f) where
  arbitrary = uncurry ProducePrices <$> arbitrary
  shrink (ProducePrices x y) = uncurry ProducePrices <$> shrink (x, y)

roundUpToZero :: Float -> Float
roundUpToZero x | x < 0 = 0
                | otherwise = x

roundWithin :: Float -> Float -> Float -> Float
roundWithin (abs -> range) normal value
  | minv > value = minv
  | maxv < value = maxv
  | otherwise = value
  where minv = normal - range
        maxv = normal + range

withinRange :: Float -> Float -> Float -> Bool
withinRange range x y = abs range >= abs (x - y)

class Wrapper f where
  wrap :: a -> f a
  unwrap :: f a -> a
instance Wrapper Identity where
  wrap = Identity
  unwrap = runIdentity
instance Wrapper Negative where
  wrap = Negative
  unwrap = getNegative
instance Wrapper NonNegative where
  wrap = NonNegative
  unwrap = getNonNegative

type QCC (c :: * -> Constraint) (f :: * -> *) = (forall a. c a => c (f a) :: Constraint)

range2 :: Float
range2 = 0.5

limePrice2 :: Wrapper f => Lens' (ProducePrices f) Float
limePrice2 = makeLens2 lemonPrice limePrice

lemonPrice2 :: Wrapper f => Lens' (ProducePrices f) Float
lemonPrice2 = makeLens2 limePrice lemonPrice

makeLens2 :: Wrapper f
          => Lens' (ProducePrices f) Float
          -> Lens' (ProducePrices f) Float
          -> Lens' (ProducePrices f) Float
makeLens2 otherLens thisLens = lens (^. thisLens)
  \s x -> s & otherLens %~ roundWithin range2 x & otherLens .~ x

spec :: Spec
spec = do
  describe "1. We’re handling a system for pricing our local grocery store’s citrus produce! Our first job is to write lenses for setting the prices of limes and lemons. Write lenses for limePrice and lemonPrice which prevent negative prices by rounding up to 0 (we’re okay with given produce out for free, but certainly aren’t going to pay others to take it)." do
    describe "never < 0" do
      describe "view" do
        it "lime" $ property \(pp :: ProducePrices Negative) -> pp ^. limePrice `shouldBe` 0
        it "lemon" $ property \(pp :: ProducePrices Negative) -> pp ^. lemonPrice `shouldBe` 0

      describe "set" do
        it "lime" $ property \(pp, Negative s) ->
          (pp & limePrice .~ s) `shouldBe` pp {_limePrice = Identity 0}
        it "lemon" $ property \(pp, Negative s) ->
          (pp & lemonPrice .~ s) `shouldBe` pp {_lemonPrice = Identity 0}

    describe "equal when >= 0" do
      describe "lime" do
        let l :: Lens' (ProducePrices NonNegative) Float
            l = limePrice
        it "set get law 1" $ expectFailure $ setGetCheck l
        it "get set law 2" $ getSetCheck l
        it "set set law 3" $ setSetCheck l

      describe "lemon" do
        let l :: Lens' (ProducePrices NonNegative) Float
            l = lemonPrice
        it "set get law 1" $ expectFailure $ setGetCheck l
        it "get set law 2" $ getSetCheck l
        it "set set law 3" $ setSetCheck l

  describe "2. The owner has informed us that it’s VERY important that the prices of limes and lemons must NEVER be further than 50 cents apart or the produce world would descend into total chaos. Update your lenses so that when setting lime-cost the lemon-cost is rounded to within 50 cents; (and vice versa)." do
    describe "always withinRange 0.5" do
      xit "lime" $ property \(p :: ProducePrices Identity, v) -> let setted = p & limePrice2 .~ v
        in withinRange range2 (setted ^. limePrice2) (setted ^. lemonPrice2)

