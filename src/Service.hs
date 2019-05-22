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

module Service where

import Nat
import Data.IORef
import Free      as F
import KindaFree as KF

import Control.Monad.Reader (ReaderT, runReader)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)

data ServiceF a = Get Int (Int -> a)
                | Set Int Int a

deriving instance (Functor ServiceF)

interpret :: IORef [Int] -> ServiceF a -> IO a
interpret ref (Get k c)   = do
  val <- readIORef ref
  return $ c (val !! k)
interpret ref (Set k v c) = do
  val <- readIORef ref
  let newVal = take k val <> [v] <> drop (k + 1) val
  writeIORef ref newVal
  return c

interpreter :: IO (ServiceF :~> IO, IORef [Int])
interpreter = do
  ref <- newIORef [0, 0]
  return (Nat $ interpret ref, ref)

---

getF :: Int -> F.Free ServiceF Int
getF key = F.Suspend (Get key return)

setF :: Int -> Int -> F.Free ServiceF ()
setF key val = F.Suspend (Set key val (pure ()))

programF :: F.Free ServiceF ()
programF = do
  val <- getF 0
  setF 0 (val + 1)
  setF 1 (val + 2)

----

setKF :: Int -> Int -> KF.Free ServiceF ()
setKF key val = KF.suspend (Set key val ())

getKF :: Int -> KF.Free ServiceF Int
getKF key = KF.suspend (Get key id)

programKF :: KF.Free ServiceF ()
programKF = do
  val <- getKF 0
  setKF 0 (val + 1)
  setKF 1 (val + 2)

----

data ServiceA f = ServiceA {
    getA :: Int -> f Int,
    setA :: Int -> Int -> f ()
}

-- data FreeA f a = FreeA {
--   runFreeA :: forall g. Monad g => f g -> g a
-- }

programA :: forall g. Monad g => ServiceA g -> g ()
programA service = do
  val <- (service `getA`) 0
  (service `setA`) 0 (val + 1)
  (service `setA`) 1 (val + 2)

interpretA :: IORef [Int] -> ServiceA IO
interpretA ref = ServiceA {
  getA = \k -> do
    val <- readIORef ref
    return $ (val !! k)
  ,
  setA = \k v -> do
    val <- readIORef ref
    let newVal = take k val <> [v] <> drop (k + 1) val
    writeIORef ref newVal
}

interpreterA :: IO (ServiceA IO, IORef [Int])
interpreterA = do
  ref <- newIORef [0, 0]
  return (interpretA ref, ref)

---

class ServiceC f where
   getC :: Int -> f Int
   setC :: Int -> Int -> f ()

programC :: Monad g => ServiceC g => g ()
programC = do
  val <- getC 0
  setC 0 (val + 1)
  setC 1 (val + 2)

-- instance ServiceC (ReaderT (IORef [Int]) IO) where
--   getC k = do
--     ref <- ask
--     val <- liftIO $ readIORef ref
--     return $ (val !! k)
--   setC k v = do
--     ref <- ask
--     val <- liftIO $ readIORef ref
--     let newVal = take k val <> [v] <> drop (k + 1) val
--     liftIO $ writeIORef ref newVal

instance ServiceC (ReaderT (IORef [Int]) IO) where
  getC k = do
    ref <- ask
    val <- liftIO $ readIORef ref
    let result = val !! k
    liftIO $ print $ "Get(" <> show k <> ") @" <>
      show val <> " -> " <> show result
    return result
  setC k v = do
    ref <- ask
    val <- liftIO $ readIORef ref
    let newVal = take k val <> [v] <> drop (k + 1) val
    liftIO $ print $ "Set(" <> show k <> ", " <> show v
      <> ") @" <> show val <> " -> " <> show newVal
    liftIO $ writeIORef ref newVal