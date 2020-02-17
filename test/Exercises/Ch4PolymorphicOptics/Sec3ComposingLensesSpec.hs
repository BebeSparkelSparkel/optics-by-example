
module Exercises.Ch4PolymorphicOptics.Sec3ComposingLensesSpec where

import Control.Lens
import Test.Hspec


data Two
data Three = Three deriving (Show, Eq)
data Five
data Eight

data Bandersnatch
data Boojum
data Chumble
data Foob
data Gazork
data Grug
data Jabberwock
data JubJub
data Mog
data Pubbawup
data Snark
data Spuzz
data Trowlg
data Wattoom
data Yakka
data Zink

spec :: Spec
spec = do
  it "1. Fill in the blank with the appropriate composition of tuple lenses in the following statement:" do
    let l = _2 . _1 . _2
    view l ("Ginerva", (("Galileo", "Waldo"), "Malfoy")) `shouldBe` "Waldo"

  -- xit "2. Given the following lens types, fill in the missing type of mysteryDomino" do
  --   let fiveEightDomino :: Lens' Five Eight
  --       fiveEightDomino = undefined
  --   let mysteryDomino :: Lens' Eight Two
  --       mysteryDomino = undefined
  --   let twoThreeDomino :: Lens' Two Three
  --       twoThreeDomino = undefined
  --   let dominoTrain :: Lens' Five Three
  --       dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino
  --   undefined ^. dominoTrain `shouldBe` Three

  -- it "3. Using what you know about how lenses work under the hood; rewrite the following signature as a polymorphic lens of the form: Lens s t a b. Then identify each animal as one of: pre-action structure, post-action structure, pre-action focus, post-action focus. 
  -- Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)" do
  --   pre-action structure: Platypus
  --   post-action structure: BabySloth
  --   pre-action focus: Armadillo
  --   post-action focus: Hedgehog

  -- it "4. Find a way to compose ALL of the following lenses together into one big path using each exactly once. What’s the type of the resulting lens?" do
  --   let spuzorktrowmble    ::  Lens  Chumble       Spuzz       Gazork        Trowlg
  --       spuzorktrowmble = undefined
  --   let gazorlglesnatchka  ::  Lens  Gazork        Trowlg      Bandersnatch  Yakka
  --       gazorlglesnatchka = undefined
  --   let zinkattumblezz     ::  Lens  Zink          Wattoom     Chumble       Spuzz
  --       zinkattumblezz = undefined
  --   let gruggazinkoom      ::  Lens  Grug          Pubbawup    Zink          Wattoom
  --       gruggazinkoom = undefined
  --   let banderyakoobog     ::  Lens  Bandersnatch  Yakka       Foob          Mog
  --       banderyakoobog = undefined
  --   let boowockugwup       ::  Lens  Boojum        Jabberwock  Grug          Pubbawup
  --       boowockugwup = undefined
  --   let snajubjumwock      ::  Lens  Snark         JubJub      Boojum        Jabberwock
  --       snajubjumwock = undefined
  --   snajubjumwock . boowockugwup . gruggazinkoom . zinkattumblezz . spuzorktrowmble . gazorlglesnatchka . banderyakoobog
