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

module Free where

import Data.IORef
import Nat

data Free f a = Pure a | Suspend (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure a)      = Pure $ f a
  fmap f (Suspend ffa) = Suspend $ (fmap f) <$> ffa

instance Functor f => Applicative (Free f) where
  pure = Pure

  ff <*> fa = do
    f <- ff
    a <- fa
    pure $ f a

instance Functor f => Monad (Free f) where
  return = Pure

  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (Pure a)      >>= f = f a
  (Suspend ffa) >>= f = Suspend $ (>>= f) <$> ffa

runFree :: Monad g => (f :~> g) -> Free f a -> g a
runFree (Nat eval) (Pure a)      = return a
runFree (Nat eval) (Suspend ffa) =
  (>>= id) $ runFree (Nat eval) <$> eval ffa