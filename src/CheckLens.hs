module CheckLens where

import Control.Lens
import Test.QuickCheck
import Test.Hspec

type LensCheck s a =
  ( Arbitrary s, Arbitrary a
  , Eq s, Eq a
  , Show s, Show a
  ) => Lens' s a -> Property

-- Law 1: You get back what you set (set-get).
setGetCheck :: LensCheck s a
setGetCheck l = property \(s, v) -> view l (set l v s) `shouldBe` v

-- Law 2: Setting back what you got doesn’t do anything (get-set).
getSetCheck :: (Arbitrary s, Eq s, Show s) => Lens' s a -> Property
getSetCheck l = property \s -> set l (view l s) s `shouldBe` s

-- Law 3: Setting twice is the same as setting once (set-set).
setSetCheck :: LensCheck s a
setSetCheck l = property \(s, v) -> set l v (set l v s) `shouldBe` set l v s

