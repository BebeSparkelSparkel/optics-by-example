{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch10Isos.Sec8LawsSpec where

import CheckIso
import Control.Lens
import Test.Hspec
import qualified Data.Map as M
import Test.QuickCheck
import Data.List as L

default (Int)


spec :: Spec
spec = do
  it "1. The following iso is unlawful; provide a counter example which shows that it breaks the law." do
    let mapList :: Ord k => Iso' (M.Map k v) [(k, v)]
        mapList = iso M.toList M.fromList
    let counterData = [(1,1),(1,2)]
    (counterData & from mapList %~ id) `shouldNotBe` counterData

  it "2. Is there a lawful implementation of the following iso? If so, implement it, if not, why not?" do
    let nonEmptyList :: Iso [a] [b] (Maybe (NonEmptyList a)) (Maybe (NonEmptyList b))
        nonEmptyList = iso
          (\case [] -> Nothing
                 xs -> Just $ NonEmpty xs )
          (\case Just nel -> getNonEmpty nel
                 Nothing -> mempty )
    checkIso nonEmptyList

  it "3. Is there a lawful implementation of an iso which ‘sorts’ a list of elements? If so, implement it, if not, why not?" do
    let sorted :: Ord a => Iso' [a] [a]
        sorted = involuted sort
    expectFailure $ checkIso sorted

  describe "4. What about the following iso which pairs each element with an Int which remembers its original position in the list. Is this a lawful iso? Why or why not? If not, try to find a counter-example." do
    let sorted :: (Ord a) => Iso' [a] [(Int, a)]
        sorted = iso to' from'
          where
            to' xs = L.sortOn snd $ zip [0..] xs
            from' xs = fmap snd $ L.sortOn fst xs

    it "counter-example" do
      let counterData = [(1,1),(1,2)]
      (counterData & from sorted %~ id) `shouldNotBe` counterData

