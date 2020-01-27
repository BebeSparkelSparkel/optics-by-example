{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch5Operators.Sec9ExercisesOperatorsSpec where

import Test.Hspec
import Control.Lens
import Data.Char (toUpper)


data Gate = Gate
  { _open :: Bool
  , _oilTemp :: Float
  } deriving (Show, Eq)

data Army = Army
  { _archers :: Int
  , _knights :: Int
  } deriving (Show, Eq)

data Kingdom = Kingdom
  { _name :: String
  , _army :: Army
  , _gate :: Gate
  } deriving (Show, Eq)

makeLenses ''Gate
makeLenses ''Army
makeLenses ''Kingdom



spec :: Spec
spec = do
  describe "1. Consider the laundry list of types. Write a chain of expressions using infix operators to get from the start state duloc to each of the following goal states. Hard mode: Try doing it without using %∼ or .∼!" do
    let duloc :: Kingdom
        duloc = Kingdom
          { _name = "Duloc"
          , _army = Army
            { _archers = 22
            , _knights = 14
            }
          , _gate = Gate
            { _open = True
            , _oilTemp = 10.0
            }
          }

    describe "goalA" do
      let goal = Kingdom
            { _name = "Duloc: a perfect place"
            , _army = Army
              { _archers = 22
              , _knights = 42
              }
            , _gate = Gate
              { _open = False
              , _oilTemp = 10.0
              }
            }
      it "standard" do
        let res = duloc
              & name .~ "Duloc: a perfect place"
              & army . knights .~ 42
              & gate . open .~ False
        res `shouldBe` goal
      it "hard" do
        let res = duloc
              & name <>~ ": a perfect place"
              & army . knights *~ 3
              & gate . open &&~ False
        res `shouldBe` goal

    describe "goalB" do
      let goal = Kingdom
            { _name = "Dulocinstein"
            , _army = Army
              { _archers = 17
              , _knights = 26
              }
            , _gate = Gate
              { _open = True
              , _oilTemp = 100.0
              }
            }
      it "standard" do
        let res = duloc
              & name .~ "Dulocinstein"
              & army . archers .~ 17
              & army . knights .~ 26
              & gate. oilTemp .~ 100
        res `shouldBe` goal
      it "hard" do
        let res = duloc
              & name <>~ "instein"
              & army . archers -~ 5
              & army . knights +~ 12
              & gate . oilTemp *~ 10
        res `shouldBe` goal

    -- Bonus: Good luck with this one! Notice the tuple around the finished Kingdom! Also; there definitely aren’t any typos! This is your goal. You’ve got all the tools you need to figure it out. Let’s see what you can do.
    describe "goalC" do
      let goal :: (String, Kingdom)
          goal =
            ( "Duloc: Home"
            , Kingdom
              { _name = "Duloc: Home of the talking Donkeys"
              , _army = Army
                { _archers = 22
                , _knights = 14
                }
              , _gate = Gate
                { _open = True
                , _oilTemp = 5.0
                }
              }
            )
      it "standard" do
        let res = ((), duloc)
              & _1 .~ "Duloc: Home"
              & _2 . name .~ "Duloc: Home of the talking Donkeys"
              & _2 . gate . oilTemp .~ 5
        res `shouldBe` goal
      it "hard" do
        let res = duloc
              & gate . oilTemp //~ 2
              & name <>~ ": Home"
              & name <<<>~ " of the talking Donkeys"
        res `shouldBe` goal

  describe "2. Enter the appropriate operator in the undefined slot to make each code example consistent" do
    it "A" $ ((False, "opossums") & _1 ||~ True) `shouldBe` (True, "opossums")
    it "B" $ (2 & id *~ 3) `shouldBe` (6 :: Int)
    it "C" do
      let res = ((True, "Dudley"), 55.0 :: Double)
            & _1 . _2 <>~ " - the worst"
            & _2 -~ 15
            & _2 //~ 2
            & _1 . _2 %~ map toUpper
            & _1 . _1 .~ False
      res `shouldBe` ((False,"DUDLEY - THE WORST"),20.0)
    
  it "3. Name a lens operator that takes only two arguments" $
    "^." `shouldBe` "^."

  it "4. What’s the type signature of %∼? Try to figure it without checking! Look at the examples above if you have to." $
    "Lens s t a b -> (a -> b) -> s -> t" `shouldBe` "Lens s t a b -> (a -> b) -> s -> t"
