module CheckIso where

import Control.Lens
import Test.Hspec
import Test.QuickCheck


checkIso :: (Show s, Show a, Eq s, Eq a, Arbitrary s, Arbitrary a) => Iso' s a -> Property
checkIso i = property \(s :: s) (a :: a) -> do
  (s & i %~ id) `shouldBe` s
  (a & from i %~ id) `shouldBe` a
  
