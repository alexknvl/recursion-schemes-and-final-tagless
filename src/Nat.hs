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

module Nat where

infixr 0 :~>
newtype (:~>) f g = Nat {
    runNat :: forall a. f a -> g a
}