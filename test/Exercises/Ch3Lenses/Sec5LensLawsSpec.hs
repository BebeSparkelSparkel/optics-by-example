module Exercises.Ch3Lenses.Sec5LensLawsSpec where

import Test.Hspec
import Control.Lens
import CheckLens
import Test.QuickCheck
import Fun ()


spec :: Spec
spec = do
  describe "1. Implement a lens which breaks the second and/or third law. That’s get-set and set-set respectively." do
    describe "fails second law" do
      let l :: Lens' (Int, Char) Int
          l = lens fst \s v -> bimap (const v) (const 'a') s
      it "passes first" $ setGetCheck l
      it "fails second" $ expectFailure $ getSetCheck l
      it "passes third" $ setSetCheck l

    describe "fails thrid law" do
      -- https://www.reddit.com/r/haskell/comments/epq09y/failing_lens_laws/
      let l :: Lens' Int Int
          l = lens (+ 1) (\_ y -> y - 1)
      it "passes first" $ setGetCheck l
      it "passes second" $ getSetCheck l
      xit "fails third" $ expectFailure $ setSetCheck l

    describe "fails second and thrid law" do
      let l :: Lens' (Char, Int) Char
          l = lens fst \s v -> bimap (const v) (+ 1) s
      it "passes first" $ setGetCheck l
      it "fails second" $ expectFailure $ getSetCheck l
      it "fails third" $ expectFailure $ setSetCheck l

  describe "2. Test the get-set and set-set laws for the msg lens we wrote this chapter. Does it pass these laws?" do
    let msg :: Lens' Err String
        msg = lens getMsg setMsg
          where
            getMsg (ReallyBadError message) = message
            getMsg (ExitCode _) = ""
            setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
            setMsg (ExitCode n) _ = ExitCode n
    it "fails first because the value may not exist" $ expectFailure $ setGetCheck msg
    it "passes second" $ getSetCheck msg
    it "passes third" $ setSetCheck msg

  describe "3. There’s a different way we could have written the msg lens such that it would PASS the set-get law and the set-set law, but fail get-set. Implement this other version." do
    let msg :: Lens' Err String
        msg = lens getMsg setMsg
          where
            getMsg (ReallyBadError m) = m
            getMsg (ExitCode _) = ""
            setMsg _ m = ReallyBadError m
    it "passes first" $ setGetCheck msg
    it "fails second" $ expectFailure $ getSetCheck msg
    it "passes third" $ setSetCheck msg

  -- describe "4. Think up a new lens which is still useful even though it breaks a law or two." do

  describe "5. BONUS (this one is tricky): Live a little; write a lens which violates ALL THREE LAWS" do
    let l :: Lens' (Int, Int) Int
        l = lens fst \s _ -> bimap (const 1) (+ 2) s
    it "fails first" $ expectFailure $ setGetCheck l
    it "fails second" $ expectFailure $ getSetCheck l
    it "fails third" $ expectFailure $ setSetCheck l

  describe "6. BONUS (another tricky one): Can you write a lawful lens for the following type: Builder" do
    let l :: Lens' Builder String
        l = lens getStr setStr
          where
            getStr (Builder (s:_) _) = s
            getStr _ = ""
            setStr b@(Builder [] _) "" = b
            setStr b@(Builder (getTail -> ss) _) s = b {_context = s:ss}
            getTail (_:t) = t
            getTail [] = []
    it "passes first" $ withMaxSuccess 10 $ setGetCheck l
    it "passes second" $ withMaxSuccess 10 $ getSetCheck l
    it "passes third" $ withMaxSuccess 10 $ setSetCheck l

data Err = ReallyBadError { _msg :: String }
         | ExitCode { _code :: Int }
  deriving (Show, Eq)
instance Arbitrary Err where
  arbitrary = oneof [ ReallyBadError <$> arbitrary
                    , ExitCode <$> arbitrary
                    ]
  shrink (ReallyBadError s) = ReallyBadError <$> shrink s
  shrink (ExitCode s) = ExitCode <$> shrink s

data Builder = Builder
  { _context :: [String]
  , _build :: [String] -> String
  } deriving (Eq)

instance Show Builder where
  show (Builder {_context}) = "Builder " <> show _context

instance Arbitrary Builder where
  arbitrary = do
    _context <- arbitrary
    _build <- applyFun <$> arbitrary
    pure $ Builder {..}
  shrink (Builder {_context, _build}) = flip Builder _build <$> shrink _context

