{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch7Traversals.Sec4TraversalActionsSpec where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Char (toUpper, toLower)
import Test.Hspec
import Test.Hspec.SmallCheck
import GHC.Generics
import Test.SmallCheck.Series

default (Int)


data User = User
  { _name :: String
  , _age :: Int
  } deriving (Show, Eq, Ord, Generic)
instance Monad m => Serial m User
makeLenses ''User

data Account = Account
  { _id :: String
  , _user :: User
  } deriving (Show, Eq, Ord, Generic)
instance Monad m => Serial m Account
makeLenses ''Account

newtype WithinAgeRange = WithinAgeRange Account deriving Show
instance Monad m => Serial m WithinAgeRange where
  series = do
    acc <- series
    Positive ageVal <- limit 149 series
    let acc' = acc & user . age .~ ageVal
    pure $ WithinAgeRange acc'

newtype OutOfRange = OutOfRange Account deriving Show
instance Monad m => Serial m OutOfRange where
  series = do
    acc <- series
    ageVal <- (negate . getNonNegative <$> series) \/
              ((+ 150) . getNonNegative <$> series)
    let acc' = acc & user . age .~ ageVal
    pure $ OutOfRange acc'

spec :: Spec
spec = do
  describe "1. Fill in the blanks, you know the drill" do
    it "A" do
      let d :: (Maybe Int, String)
          d = (Nothing, "Rosebud")
      let r :: Maybe (Int, String)
          r = Nothing
      sequenceAOf _1 d `shouldBe` r

    it "B" do
      let d :: [(String, Int)]
          d = [("ab", 1), ("cd", 2)]
      let r :: [[(Char, Int)]]
          r = [ [('a', 1), ('c', 2)]
              , [('a', 1), ('d', 2)]
              , [('b', 1), ('c', 2)]
              , [('b', 1), ('d', 2)]
              ]
      sequenceAOf (traversed . _1) d `shouldBe` r

    it "C" do
      -- The ZipList effect groups elements by position in the list
      let d = [ZipList [1, 2], ZipList [3, 4]]
      let r = ZipList {getZipList = [[1,3],[2,4]]}
      let s :: ( s ~ [ZipList Int]
               , t ~ [Int]
               , f ~ ZipList
               , b ~ Int
               ) => LensLike f s t (f b) b -> s -> f t 
          s = sequenceAOf
      s traversed d `shouldBe` r

    it "D" do
      let t = beside traversed each
      let result = traverseOf t (\n -> modify (+n) >> get) ([1, 1, 1], (1, 1))
      runState result 0 `shouldBe` (([1,2,3],(4,5)), 5)

  describe "2. Rewrite the following using the infix-operator for traverseOf (%%~)" do
    it "A" do
      let d = ("ab", True)
      let s = [ ("ab",True)
              , ("aB",True)
              , ("Ab",True)
              , ("AB",True) ]
      let r = d & _1 . traversed %%~ \c -> [toLower c, toUpper c]
      r `shouldBe` s

    it "B" do
      let d = [('a', True), ('b', False)]
      let s = [ [('a',True), ('b',False)]
              , [('a',True), ('B',False)]
              , [('A',True), ('b',False)]
              , [('A',True), ('B',False)] ]
      let r = d & traversed . _1 %%~ \c -> [toLower c, toUpper c]
      r `shouldBe` s

  describe "3. Given the following data definitions, write a validation function which uses traverseOf or %%∼ to validates that the given user has an age value above zero and below 150. Return an appropriate error message if it fails validation." do
    let validateError = Left "not within range"
    let validateAge :: Account -> Either String Account
        validateAge acc = if userAge > 0 && userAge < 150
          then Right acc
          else validateError
          where userAge = acc ^. user . age 

    it "Right if > 0 and < 150" $ property \(WithinAgeRange acc) ->
      validateAge acc `shouldBe` Right acc

    it "Left if <= 0 and >= 150" $ property \(OutOfRange acc) ->
      validateAge acc `shouldBe` validateError

