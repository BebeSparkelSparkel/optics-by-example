
module Exercises.Ch7Traversals.Sec5CustomTraversalsSpec where

import Control.Applicative
import Control.Lens
import Test.Hspec
import Test.Hspec.SmallCheck
import GHC.Generics
import Test.SmallCheck.Series

default (Int)

data Transaction =
    Withdrawal {_amount :: Int}
  | Deposit {_amount :: Int}
  deriving (Show, Eq, Generic)
makeLenses ''Transaction
instance Monad m => Serial m Transaction

spec :: Spec
spec = do
  describe "1. Rewrite the amount transaction lens manually as the following traversal" do
    -- let amountT :: Traversal' Transaction Int
    -- let amountT :: Traversal Transaction Transaction Int Int
    let amountT :: Applicative f => (Int -> f Int) -> Transaction -> f Transaction
        amountT f = \case
          Withdrawal x -> Withdrawal <$> f x
          Deposit x -> Deposit <$> f x

    it "traverse values" $ property \t ->
      t ^.. amountT `shouldBe` t ^.. amount

    it "reconstructs" $ property \t m ->
      (t & amountT *~ m) `shouldBe` (t & amount *~ m)

  describe "2. Reimplement the both traversal over tuples" do
    -- let bothT :: Traversal (a, a) (b, b) a b
    let bothT :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
        bothT f (x, y) = liftA2 (,) (f x) (f y)

    it "traverse values" $ property \(t :: (Int, Int)) ->
      t ^.. bothT `shouldBe` t ^.. both

    it "reconstructs" $ property \(t :: (Int, Int)) m ->
      (t & bothT *~ m) `shouldBe` (t & both *~ m)

  describe "3. Write the following custom traversal" do
    -- It should focus the amount of the transaction, but should reflect the change that the transaction causes to the balance of the account. That is, Deposits should be a positive number, but Withdrawals should be negative. The traversal should not change the underlying representation of the data.
    -- let transactionDelta :: Traversal' Transaction Int
    let transactionDelta :: Applicative f => (Int -> f Int) -> Transaction -> f Transaction
        transactionDelta f = \case
          Withdrawal x -> Withdrawal . negate <$> f (negate x)
          Deposit x -> Deposit <$> f x
    it "A" ((Deposit 10 ^? transactionDelta) `shouldBe` Just 10)
    it "B" ((Withdrawal 10 ^? transactionDelta) `shouldBe` Just (-10))
    it "C" ((Deposit 10 & transactionDelta .~ 15) `shouldBe` Deposit 15)
    it "D" ((Withdrawal 10 & transactionDelta .~ (-15)) `shouldBe` Withdrawal 15)
    it "E" ((Deposit 10 & transactionDelta +~ 5) `shouldBe` Deposit 15)
    it "F" ((Withdrawal 10 & transactionDelta +~ 5) `shouldBe` Withdrawal 5)

  describe "4. Implement left" do
    -- let leftT :: Traversal (Either a b) (Either a' b) a a'
    let leftT :: Applicative f => (a -> f a') -> Either a b -> f (Either a' b)
        leftT f = \case
          Left x -> Left <$> f x
          Right x -> pure $ Right x

    it "traverse values" $ property \(x :: Either Word Int) ->
      x ^.. leftT `shouldBe` x ^.. _Left

    it "reconstructs" $ property \(x :: Either Word Int) y ->
      (x & leftT +~ y) `shouldBe` (x & _Left +~ y)

  describe "5. BONUS: Reimplement the beside traversal" do
    -- Hint: You can use traverseOf or %%∼ to help simplify your implementation!
    describe "liftA2" do
      let besideT :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
          besideT t t' f (x, y) = liftA2 (,) (t f x) (t' f y)

      it "traverse values" $ property \(t :: (Int, Int)) ->
        t ^.. besideT id id `shouldBe` t ^.. beside id id

      it "reconstructs" $ property \(t :: (Int, Int)) m ->
        (t & besideT id id *~ m) `shouldBe` (t & beside id id *~ m)

    describe "traverseOf" do
      let besideT :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
          besideT t t' f (x, y) = liftA2 (,)
            (traverseOf t f x)
            (traverseOf t' f y)

      it "traverse values" $ property \(t :: (Int, Int)) ->
        t ^.. besideT id id `shouldBe` t ^.. beside id id

      it "reconstructs" $ property \(t :: (Int, Int)) m ->
        (t & besideT id id *~ m) `shouldBe` (t & beside id id *~ m)

    describe "%%~" do
      let besideT :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
          besideT t t' f (x, y) = liftA2 (,)
            (x & t %%~ f)
            (y & t' %%~ f)

      it "traverse values" $ property \(t :: (Int, Int)) ->
        t ^.. besideT id id `shouldBe` t ^.. beside id id

      it "reconstructs" $ property \(t :: (Int, Int)) m ->
        (t & besideT id id *~ m) `shouldBe` (t & beside id id *~ m)

