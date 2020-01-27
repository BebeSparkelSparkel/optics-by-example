module Exercises.Ch4PolymorphicOptics.Sec2WhenWoWeNeedPolymorphicLensesSpec where

import Control.Lens
import Control.Arrow
import CheckLens
import Test.Hspec
import Test.QuickCheck (Arbitrary(..))
import Fun ()


newtype Vorpal x = Vorpal {runVorpal :: x}
  deriving (Show, Eq)
  deriving newtype Arbitrary

data Preferences a = Preferences
  { _best :: a
  , _worst :: a
  } deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Preferences a) where
  arbitrary = uncurry Preferences <$> arbitrary
  shrink (Preferences x y) = uncurry Preferences <$> shrink (x, y)

data Preferneces1 a b = Preferneces1
  { _best1 :: a
  , _worst1 :: b
  } deriving (Show, Eq)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Preferneces1 a b) where
  arbitrary = uncurry Preferneces1 <$> arbitrary
  shrink (Preferneces1 x y) = uncurry Preferneces1 <$> shrink (x, y)

data Result e = Result
  { _lineNumber :: Int
  , _result :: Either e String
  } deriving (Show, Eq)
instance Arbitrary e => Arbitrary (Result e) where
  arbitrary = uncurry Result <$> arbitrary
  shrink (Result x y) = uncurry Result <$> shrink (x, y)

newtype Predicate a = Predicate {getPredicate :: (a -> Bool)} 
  deriving Eq
  deriving newtype Arbitrary
instance Show (Predicate a) where show = const "Predicate"

spec :: Spec
spec = do
  describe "1. Write the type signature of the polymorphic lens which would allow changing a Vorpal x to a Vorpal y." do
    let l :: forall a b. Lens (Vorpal a) (Vorpal b) a b
        l = lens runVorpal \_ x -> Vorpal x
    it "passes first" $ setGetCheck (l @Bool)
    it "passes second" $ getSetCheck (l @Bool)
    it "passes third" $ setSetCheck (l @Bool)

  describe "2. Find one possible way to write a polymorphic lens which changes the type of the best and worst fields in the Preferences type above. You’re allowed to change the type of the lenses or alter the type itself!" do
    describe "option 0" do
      let bestWorst :: Lens (Preferences a) (Preferences b) (a, a) (b, b)
          bestWorst = lens (_best &&& _worst) (const $ uncurry Preferences)
      it "passes first" $ setGetCheck (bestWorst @Bool) 
      it "passes second" $ getSetCheck (bestWorst @Bool) 
      it "passes third" $ setSetCheck (bestWorst @Bool) 

    describe "option 1" do
      let best :: forall a b c. Lens (Preferneces1 a b) (Preferneces1 c b) a c
          best = lens _best1 \p x -> p {_best1 = x}
      it "passes first" $ setGetCheck (best @Bool @Char) 
      it "passes second" $ getSetCheck (best @Bool @Char) 
      it "passes third" $ setSetCheck (best @Bool @Char) 

      let worst :: forall a b c. Lens (Preferneces1 a b) (Preferneces1 a c) b c
          worst = lens _worst1 \p x -> p {_worst1 = x}
      it "passes first" $ setGetCheck (worst @Bool @Char)
      it "passes second" $ getSetCheck (worst @Bool @Char)
      it "passes third" $ setSetCheck (worst @Bool @Char)

  describe "3. We can change type of more complex types too. What is the type of a lens which could change the type variable of Result" do
    let l :: Lens (Result a) (Result b) (Either a String) (Either b String)
        l = lens _result \s x -> s {_result = x}
    it "passes first" $ setGetCheck (l @Bool)
    it "passes second" $ getSetCheck (l @Bool)
    it "passes third" $ setSetCheck (l @Bool)

  describe "4. It’s thinking time! Is it possible to change more than one type variable at a time using a polymorphic lens?" do
    let l :: forall a b c d e. Lens (a,b,c) (d,e,c) (a,b) (d,e)
        l = lens (\(x,y,_) -> (x,y)) \(_,_,z) (x,y) -> (x,y,z)
    it "passes first" $ setGetCheck (l @Bool @Char @Int)
    it "passes second" $ getSetCheck (l @Bool @Char @Int)
    it "passes third" $ setSetCheck (l @Bool @Char @Int)

  describe "5. BONUS Come up with some sort of lens to change from a Predicate a to a Predicate b" do
    let l :: forall a b. Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
        l = lens getPredicate (const Predicate)
    it "passes first" $ setGetCheck (l @Bool)
    it "passes second" $ getSetCheck (l @Bool)
    it "passes third" $ setSetCheck (l @Bool)

