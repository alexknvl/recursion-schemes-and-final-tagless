{-# LANGUAGE
  ExistentialQuantification
  , ConstraintKinds
  , KindSignatures
  , RankNTypes
  , ScopedTypeVariables
  , DeriveFunctor
  , DeriveTraversable
  , BangPatterns
  , FunctionalDependencies
  , InstanceSigs
  , StandaloneDeriving
  , UnicodeSyntax
  , ImpredicativeTypes
  , FlexibleInstances
  , FlexibleContexts
  , ScopedTypeVariables
  , LambdaCase
  , BlockArguments
  , TypeOperators
#-}

module KindaFree where

import Data.Kind (Constraint)
import Data.IORef
import Nat

data Free (f :: * -> *) a = Free {
  runFree :: forall (g :: * -> *).
             Monad g => (f :~> g) -> g a
}

-- FreeA (f :~>) a = Free f a

data FreeA (alg :: (* -> *) -> *) a = FreeA {
  runFreeA :: forall g. Monad g => alg g -> g a
}

data FreeC (alg :: (* -> *) -> Constraint) a = FreeC {
  runFreeC :: forall g. Monad g => alg g => g a
}

suspend :: f a -> Free f a
suspend fa = Free $ \(Nat eval) -> eval fa

instance Functor f => Functor (Free f) where
  fmap f ffa = Free (\nat -> f <$> runFree ffa nat)

instance Functor f => Applicative (Free f) where
  pure a = Free (\_ -> pure a)

  ff <*> fa = do
    f <- ff
    a <- fa
    pure $ f a

instance Functor f => Monad (Free f) where
  return a = Free (\_ -> pure a)

  ffa >>= f = Free (\nat -> let gffa = f <$> (runFree ffa nat)
                                gga = (\ffa -> runFree ffa nat) <$> gffa
                            in gga >>= id)