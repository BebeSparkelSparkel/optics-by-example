module CheckTraveral where

import Test.QuickCheck
import Control.Lens
import Test.Hspec
import Data.Functor.Compose


respectPurity :: forall (f :: * -> *) s a.
               ( Arbitrary s
               , Show s
               , Eq (f s)
               , Applicative f
               )
              => Traversal' s a -> Property
respectPurity t = property \x ->
  traverseOf @f t pure x == pure x

consistentFocuses :: forall (f :: * -> *) (g :: * -> *) s a.
                   ( Applicative f
                   , Applicative g
                   , Arbitrary (f a)
                   , Arbitrary (g a)
                   , Arbitrary s
                   , CoArbitrary a
                   , Eq (g (f s))
                   , Function a
                   , Functor g
                   , forall b. Show b => Show (f b)
                   , forall b. Show b => Show (g b)
                   , Show a
                   , Show s
                   )
                  => Traversal' s a -> Property
consistentFocuses t = property \x (applyFun -> f) (applyFun -> g) -> let
  doubleTraverse = fmap (traverseOf @f t f) . traverseOf @g t g $ x
  singleTraverse = getCompose . traverseOf t (Compose . fmap f . g) $ x
  in doubleTraverse `shouldBe` singleTraverse

