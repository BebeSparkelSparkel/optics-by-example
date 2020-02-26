{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Exercises.Ch11IndexedOptics.Sec2IndexCompositionSpec where

import Control.Lens
import Test.Hspec
import qualified Data.Map as M
import Data.IORef

default (Int)


spec :: Spec
spec = do
  describe "1. Fill in the blanks" do
    it "A" do
      let fillIn = (^@..)
      let result = M.fromList [("streamResponse", False), ("useSSL", True)] `fillIn` itraversed
      let answer = [("streamResponse",False),("useSSL",True)]
      shouldBe result answer

    it "B" do
      let fillIn = (<.)
      let result = M.fromList [('a', (True, 1)), ('b', (False, 2))]
            ^@.. itraversed `fillIn` _1
      let answer = [('a', True), ('b', False)]
      shouldBe result answer

    it "C" do
      let fillIn = itraversed <.> itraversed
      let result = [ M.fromList [("Tulips", 5), ("Roses", 3)]
                   , M.fromList [("Goldfish", 11), ("Frogs", 8)] ]
                   ^@.. fillIn
      let answer = [ ((0,"Roses"), 3)
                   , ((0,"Tulips"), 5)
                   , ((1,"Frogs"), 8)
                   , ((1,"Goldfish"), 11) ]
      shouldBe result answer

    it "D" do
      let fillIn = (%@~)
      let result = [10, 20, 30] & itraversed `fillIn` (+)
      let answer = [10,21,32]
      shouldBe result answer

    it "E" do
      printed <- newIORef mempty
      let localPutStrLn :: String -> IO ()
          localPutStrLn s = modifyIORef printed (<> (s <> "\n"))
      let fillIn = itraverseOf_
      fillIn
        itraversed
        (\i s -> localPutStrLn (replicate i ' ' <> s))
        ["one", "two", "three"]
      let answer = "one\n\
                   \ two\n\
                   \  three\n"
      result <- readIORef printed
      shouldBe result answer

    it "F" do
      printed <- newIORef mempty
      let localPutStrLn :: String -> IO ()
          localPutStrLn s = modifyIORef printed (<> (s <> "\n"))
      let fillIn n s = show n <> ": " <> s
      itraverseOf_
        itraversed
        (\n s -> localPutStrLn $ fillIn n s)
        ["Go shopping", "Eat lunch", "Take a nap"]
      let answer = "0: Go shopping\n\
                   \1: Eat lunch\n\
                   \2: Take a nap\n"
      result <- readIORef printed
      shouldBe result answer

