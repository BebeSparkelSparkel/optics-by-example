{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fun where

import Test.QuickCheck
import qualified System.IO.Unsafe
import Control.Applicative
import Data.List


instance (Arbitrary a, Show a, Show b) => Show (a -> b) where
  show f = toSet inputs <> " -> " <> toSet outputs
    where inputs = System.IO.Unsafe.unsafePerformIO $ sample' arbitrary
          outputs = f <$> inputs
          toSet xs = '{' : (intercalate ", " $ show <$> xs) <> "}"

instance (Arbitrary a, Eq b) => Eq (a -> b) where
  x == y = all (liftA2 (==) x y) $
    System.IO.Unsafe.unsafePerformIO $ generate $ vectorOf 10 arbitrary

