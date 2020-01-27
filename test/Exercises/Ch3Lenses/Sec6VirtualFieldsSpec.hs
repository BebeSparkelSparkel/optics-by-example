module Exercises.Ch3Lenses.Sec6VirtualFieldsSpec where

import CheckLens
import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Tuple.HT
import Test.Hspec
import Test.QuickCheck

data User = User
  { _firstName :: String
  , _lastName :: String
  -- , _username :: String
  , _email :: String
  } deriving (Show, Eq)
makeLenses ''User

instance Arbitrary User where
  arbitrary = do
    _firstName <- arbitrary
    _lastName <- arbitrary
    _email <- arbitrary
    pure $ User {..}
  shrink b = uncurry3 User <$> shrink (b ^. firstName, b ^. lastName, b ^. email)

username :: Lens' User String
username = email

fullName :: Lens' User String
fullName = lens getter setter
  where
    getter :: User -> String
    getter = liftA2 (<>)
      (^. firstName)
      ((^. lastName) >>> \case
        "" -> ""
        x -> ' ' : x
      )
    setter u =
      -- (\str -> let 
      --   leadingSpaces = takeWhile (== ' ') str
      --   postLeadingSpaces = dropWhile (== ' ') str
      --   is
      --   span (/= ' ')
      -- ) >>>
      span (/= ' ') >>>
      (\case
        (fn, ' ':ln) -> (fn, ln)
        x -> x
      ) >>>
      \(fn, ln) -> set firstName fn $ set lastName ln u


spec :: Spec
spec = do
  -- it "1. We’ve decided we’re no longer going to have separate usernames and emails; now the email will be used in place of a username. Your task is to delete the _username field and write a replacement username lens which reads and writes from/to the _email field instead. The change should be unnoticed by those importing the module." $

  describe "2. Write a lens for the user’s fullName. It should append the first and last names when “getting”. When “setting” treat everything till the first space as the first name, and everything following it as the last name." do
    it "passes first" $ setGetCheck fullName
    xit "passes second" $ getSetCheck fullName
    it "passes third" $ setSetCheck fullName
