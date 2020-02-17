module Exercises.Ch7Traversals.Sec6TraversalLawsSpec where

import Control.Lens
import Test.Hspec
import CheckTraveral
import Test.QuickCheck

default (Int)


spec :: Spec
spec = do
  describe "1. worded is a law-breaking traversal! Determine which law it breaks and give an example which shows that it doesn’t pass the law." do
    it "respectPurity" $ expectFailure $ respectPurity @Identity worded
    -- Worded breaks the respect purity because whitespace is lost
    it "consistentFocuses" $ expectFailure $ consistentFocuses @Identity @Identity worded
    -- When white space is lost words are lost for the second traversal while if there is only one traversal the singular whitespace is conserved

  describe "2. Write a custom traversal which breaks the first law. Be as creative as you like!" do
    let t :: Traversal' String Char
        t h "" = t h " "
        t h s@(' ':_) = (:) <$> h ' ' <*> traverseOf traversed h s
        t h s = traverseOf traversed h s
    it "respectPurity" $ expectFailure $ respectPurity @Identity t
    it "consistentFocuses" $ consistentFocuses @Identity @Identity t

  describe "3. Write a custom traversal which breaks the second law. Be as creative as you like!" do
    let t :: Traversal' String Char
        t = traversed . filtered (/= 'a')
    it "respectPurity" $ respectPurity @Identity t
    it "consistentFocuses" $ expectFailure $ consistentFocuses @Identity @Identity t

  -- describe "4. For each of the following traversals, decide whether you think it’s lawful or not. If they’re unlawful come up with a counter-example for one of the laws." do
  --   describe "taking" do
      -- it "respectPurity" $ property \n ->
      --   respectPurity @Identity (taking n :: Traversal' [Word] Word)
      -- it "consistentFocuses" $ property \n ->
      --   consistentFocuses @Identity @Identity (taking n :: Traversal' [Word] Word)

    -- describe "beside" do
    --   it "respectPurity" $ respectPurity @Identity beside
    --   it "consistentFocuses" $ consistentFocuses @Identity @Identity beside

    -- describe "each" do
    --   let t :: Traversal (Int, Int, Int) (Word, Word, Word) Int Char
    --       t = each id
    --   it "respectPurity" $ respectPurity @Identity t
    --   it "consistentFocuses" $ consistentFocuses @Identity @Identity t

    -- describe "lined" do
    --   it "respectPurity" $ expectFailure $ respectPurity @Identity lined
    --   it "consistentFocuses" $ expectFailure $ consistentFocuses @Identity @Identity lined

    -- describe "traversed" do
    --   it "respectPurity" $
    --     respectPurity @Identity (traversed :: Traversal' [Word] Word)
    --   it "consistentFocuses" $
    --     consistentFocuses @Identity @Identity (traversed :: Traversal' [Word] Word)

