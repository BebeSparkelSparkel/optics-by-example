{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch9Prisms.Sec2WritingCustomPrismsSpec where

import Test.Hspec
import Control.Lens
import Data.List

default (Int)


spec :: Spec
spec = do
  -- 1. Try to write a custom prism for matching on the tail of a list: _Tail :: Prism' [a] [a].  Is this possible? Why or why not?
  -- It isn't possible since tail cannot reconstruct the origional list.

  it "2. Implement _Cons for lists using prism: _ListCons" do
    let _ListCons :: Prism [a] [b] (a, [a]) (b, [b])
        _ListCons = prism embed match
          where
            embed :: (b, [b]) -> [b]
            embed = uncurry (:)
            match :: [a] -> Either [b] (a, [a])
            match (x:xs) = Right (x, xs)
            match [] = Left []
    ([] ^? _ListCons) `shouldBe` Nothing
    ([1,2] ^? _ListCons) `shouldBe` Just (1,[2])
    (_ListCons # (1,[])) `shouldBe` [1]
    (_ListCons # (1,[2,3])) `shouldBe` [1,2,3]

  describe "BONUS 3. Implement _Cycles which detects whether a list consists of exactly ‘n’ repetitions of a pattern. It should behave as follows:" do
    let _Cycles :: Int -> Prism String String String String
        _Cycles n = prism (embed n) match
          where
            embed :: Int -> String -> String
            embed 0 _ = mempty
            embed x s = s <> embed (x - 1) s
            match :: String -> Either String String
            match s | m 0 s = Right prefix
                    | otherwise = Left s
              where 
                prefix = take (l `div` n) s
                l = length s
                m c [] | c == n = True
                m c xs = case stripPrefix prefix xs of
                  Just ys -> m (c + 1) ys
                  Nothing -> False

    it "Find the subsequence which has been repeated exactly times." $
      ("dogdogdog" ^? _Cycles 3) `shouldBe` Just "dog"

    it "The input isn't exactly 3 cycles of some input (it's 4), so we don't match" $
      ("dogdogdogdog" ^? _Cycles 3) `shouldBe` Nothing

    it "The input is exactly 3 cycles of \"a\"" $
      ("aaa" ^? _Cycles 3) `shouldBe` Just "a"

    it "The input isn't a cycle at all." $
      ("xyz" ^? _Cycles 3) `shouldBe` Nothing

    it "We can review to create cycles." $
      (_Cycles 3 # "dog") `shouldBe` "dogdogdog"

    it "We can mutate the sequence that's cycled" $
      ("dogdogdog" & _Cycles 3 .~ "cats") `shouldBe` "catscatscats"

-- • Can you implement a version of _Cycles which doesn’t depend on a specific iteration count? Why or why not?

