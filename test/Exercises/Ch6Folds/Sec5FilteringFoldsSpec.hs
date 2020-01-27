{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch6Folds.Sec5FilteringFoldsSpec where

import Test.Hspec
import Control.Lens

default (Int)

-- A data structure to represent a single card
data Card = Card
  { _name  :: String
  , _aura  :: Aura
  , _holo  :: Bool
  , _moves :: [Move]
  } deriving (Show, Eq)

-- Each card has an aura-type
data Aura = Wet | Hot | Spark | Leafy deriving (Show, Eq)

-- Cards have attack moves
data Move = Move
  { _moveName :: String
  , _movePower :: Int
  } deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

-- That’ll help me model my collection. Here are the cards I collected over the years:
deck :: [Card]
deck =
    [ Card "Skwortul" Wet False [Move "Squirt" 20]
    , Card "Scorchander" Hot False [Move "Scorch" 20]
    , Card "Seedasaur" Leafy False [Move "Allergize" 20]
    , Card "Kapichu" Spark False [Move "Poke" 10 , Move "Zap" 30]
    , Card "Elecdude" Spark False [Move "Asplode" 50]
    , Card "Garydose" Wet True [Move "Gary's move" 40]
    , Card "Moisteon" Wet False [Move "Soggy" 3]
    , Card "Grasseon" Leafy False [Move "Leaf Cut" 30]
    , Card "Spicyeon" Hot False [Move "Capsaicisize" 40]
    , Card "Sparkeon" Spark True [Move "Shock" 40 , Move "Battery" 50]
    ]

spec :: Spec
spec = do
  describe "Use a fold to answer each of the questions about my card collection:" do
    describe "List all the cards whose name starts with 'S'." do
      let result = [ "Skwortul", "Scorchander", "Seedasaur", "Spicyeon", "Sparkeon" ]

      it "filtered" $ deck ^.. folded . name . filtered ((== 'S') . head)
        `shouldBe` result

      xit "filteredBy" $ deck ^.. folded . name -- . filteredBy
        `shouldBe` result

    it "What’s the lowest attack power of all moves?" $
      minimumOf (folded . moves . folded . movePower) deck
        `shouldBe` Just 3

    describe "What’s the name of the first card which has more than one move?" do
      let result = Just "Kapichu"
      it "filtered" $
        firstOf (folded . filtered ((> 1) . length . _moves) . name) deck
          `shouldBe` result

      it "filteredBy" $
        firstOf (folded . filteredBy (moves . filtered ((> 1) . length)) . name) deck
          `shouldBe` result

    it "Are there any Hot cards with a move with more than 30 attack power?" $
      anyOf (folded . moves . folded . movePower) (> 30) deck
        `shouldBe` True

    describe "List the names of all holographic cards with a Wet aura." do
      let result = [ "Garydose" ]

      it "filtered" $
        deck ^..
          folded .
          filtered ((== True) . _holo) .
          filtered ((== Wet) . _aura) .
          name
          `shouldBe` result

      it "filteredBy" $
        deck ^..
          folded .
          filteredBy (holo . only True) .
          filteredBy (aura . only Wet) .
          name
          `shouldBe` result

    describe "What’s the sum of all attack power for all moves belonging to non-Leafy cards?" do
      let result = 303

      it "filtered" $
        sumOf (folded . filtered ((/= Leafy) . _aura) . moves . folded . movePower) deck
          `shouldBe` result

      it "filteredBy" $
        sumOf (folded . filteredBy (aura . filtered (/= Leafy)) . moves . folded . movePower) deck
          `shouldBe` result

