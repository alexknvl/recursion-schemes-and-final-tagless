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
#-}

module Main where

import Data.IORef
import Service
import Free      as F
import KindaFree as KF
import Control.Monad.Reader (ReaderT, runReaderT)

main :: IO ()
main = do
  (int, ref) <- interpreter
  F.runFree int programF
  result <- readIORef ref
  print $ result

  (int, ref) <- interpreter
  KF.runFree programKF int
  result <- readIORef ref
  print $ result

  (int', ref') <- interpreterA
  programA int'
  result <- readIORef ref'
  print $ result

  ref <- newIORef [0 :: Int, 0]
  runReaderT programC ref
  result <- readIORef ref
  print $ result