{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Science.MD.GMX.Internal.MDPBuilder where

import Science.MD.GMX.Internal.MDP
import Science.MD.GMX.Internal.MDPPArser


import Control.Applicative ((<$>))

import "mtl" Control.Monad.State



newtype MDP a = MkMDP {
      unMDP :: StateT MDPData IO a
    } deriving (Functor, Monad, MonadState MDPData, MonadIO)


entry :: MDPKey -> MDPVal -> MDP MDPData
entry k v = do
  -- TODO: warn if overwriting a key
  put =<< addEntry (newEntry k v) <$> get
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


runMDP = flip runStateT s0 . unMDP
    where s0 = emptyData


testMDP = do
  load "/tmp/sim.mdp"
  entry "dt" "0.005"
  entry "ref-t" "300"


t = putStrLn =<< toMDP . snd <$> runMDP testMDP