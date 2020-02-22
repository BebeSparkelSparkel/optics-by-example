module CheckPrism where

import Control.Lens
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Control.Monad


lawOneReviewPreview :: (Arbitrary a, Eq a, Show a) => Prism' s a -> Property
lawOneReviewPreview p = property \v -> preview p (review p v) `shouldBe` Just v

lawTwoPrismComplement :: (Arbitrary s, Eq s, Show s) => Prism s s a a -> Property
lawTwoPrismComplement p = forAll (suchThat arbitrary (isJust . preview p)) \s -> do
  let Just a = preview p s
  let s' = review p a
  s `shouldBe` s'

lawThreePassThroughReversion :: (Arbitrary s, Eq s, Show s) => Prism s s a b -> Property
lawThreePassThroughReversion p = forAll arb \s ->
  Just s `shouldBe` check s
  where
    arb = fromJust <$> suchThat (check <$> arbitrary) (/= mzero)
    check s = case matching p s of
      Left t -> case matching p t of
        Left s' -> pure s'
        _ -> mzero
      _ -> mzero

