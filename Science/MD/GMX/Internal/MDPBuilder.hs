{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Science.MD.GMX.Internal.MDPBuilder where

import Science.MD.GMX.Internal.MDP
import Science.MD.GMX.Internal.MDPPArser


import Control.Applicative ((<$>))

import "mtl" Control.Monad.State



newtype MDP a = MkMDP {
      unMDP :: StateT MDPData IO a
    } deriving (Functor, Monad, MonadState MDPData, MonadIO)

runMDP = flip runStateT s0 . unMDP
    where s0 = emptyData

-- -------------------------------------------------------------------------------- --

class RawValue a where
    type ValueT
    rawValue :: a -> ValueT

class ToKey a where
    toMDPKey :: a -> MDPKey

class ToValue a where
    toMDPVal :: a -> MDPVal

class (ToKey a, ToValue a) => ToMDPEntry a where
    toMDPEntry :: a -> MDPEntry
    toMDPEntry a = let k = toMDPKey a
                       v = toMDPVal a
                   in newEntry k v

-- -------------------------------------------------------------------------------- --

data Kelvin

newtype Temperature a = MkTemp Double deriving (Eq, Ord, Show)

mkTemperature :: Double -> Temperature Kelvin
mkTemperature = MkTemp

instance ToKey (Temperature a) where
    toMDPKey = const "ref-t"

instance ToValue (Temperature Kelvin) where
    toMDPVal (MkTemp t) = show t

instance ToMDPEntry (Temperature Kelvin)

-- -------------------------------------------------------------------------------- --

data Femtoseconds
data Picoseconds
data Nanoseconds

data Timestep a = Dt1fs | Dt2fs | Dt5fs | DtCustom Double

mkDt :: Double -> Timestep Femtoseconds
mkDt = DtCustom


instance ToKey (Timestep a) where
    toMDPKey = const "dt"

instance ToValue (Timestep Femtoseconds) where
    toMDPVal = show . (/ 1000.0) . rawValue

instance ToMDPEntry (Timestep Femtoseconds)


instance RawValue (Timestep Femtoseconds) where
    type ValueT = Double
    rawValue Dt1fs = 1
    rawValue Dt2fs = 2
    rawValue Dt5fs = 5
    rawValue (DtCustom fs) = fs


-- -------------------------------------------------------------------------------- --

data OutputFrequency a = MkOutputFreq Double

mkOutputFreq :: Integer -> Timestep Picoseconds -> OutputFrequency Nanoseconds
mkOutputFreq f dt = undefined

-- -------------------------------------------------------------------------------- --

entry' :: MDPKey -> MDPVal -> MDP MDPData
entry' k v = do
  entry (newEntry k v)

entry :: MDPEntry -> MDP MDPData
entry e = do
  -- TODO: warn if overwriting a key
  put =<< addEntry e <$> get
  get

load :: FilePath -> MDP MDPData
load p = do
  e <- liftIO $ loadMDP p
  d <- either
         (\l -> error $ "Could not parse file '" ++ p ++ "': " ++ show l)
         return
         e
  put d
  return d

dt :: Timestep Femtoseconds -> MDP MDPData
dt = entry . toMDPEntry

dtFs :: Double -> MDP MDPData
dtFs = dt . mkDt

temp :: Temperature Kelvin -> MDP MDPData
temp = entry . toMDPEntry

tempK :: Double -> MDP MDPData
tempK = temp . mkTemperature



testMDP = do
  load "/tmp/sim.mdp"
  dtFs 5
  tempK 400



t = putStrLn =<< toMDP . snd <$> runMDP testMDP