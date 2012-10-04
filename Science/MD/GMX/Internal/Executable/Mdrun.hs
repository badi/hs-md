{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Science.MD.GMX.Internal.Executable.Mdrun where

import Science.MD.GMX.Internal.ExecutableBuilder hiding (test, t)
import Science.MD.GMX.Internal.Executable.Pdb2Gmx (Pdb2Gmx, ForceField(..), Water(..), pdb2gmx)
import qualified Science.MD.GMX.Internal.Executable.Pdb2Gmx as Pdb2Gmx
import Science.MD.GMX.Internal.Executable.Grompp (Grompp, grompp, mdp)
import qualified Science.MD.GMX.Internal.Executable.Grompp as Grompp

import Science.MD.GMX.Internal.MDP
import Science.MD.GMX.Internal.MDPBuilder hiding (t)

import Control.Lens

import Control.Applicative ((<$>))

import Control.Lens

import "mtl" Control.Monad.State
import Data.Maybe (fromJust)
import System.FilePath
import System.Directory

data Mdrun = MkMdrun {
      _trr :: Maybe FilePath
    , _xtc :: Maybe FilePath
    , _cpt :: Maybe FilePath
    , _confout :: Maybe FilePath
    , _edr :: Maybe FilePath
    , _mdlog :: Maybe FilePath
    } deriving Show

makeLenses ''Mdrun

emptyMdrun :: Mdrun
emptyMdrun = MkMdrun Nothing Nothing Nothing Nothing Nothing Nothing

newtype MdrunFileChecker a = MkMdrunFileChecker {
      unMdrunFC :: StateT Mdrun IO a
    } deriving (Functor, Monad, MonadIO, MonadState Mdrun)


runMdrunFileChecker :: MdrunFileChecker a -> IO Mdrun
runMdrunFileChecker = fmap snd . flip runStateT emptyMdrun . unMdrunFC

instance GetOutputFiles Mdrun where
    getOutput c = let w = c ^. cmdWorkarea
                      checks = zip   [trr, xtc, cpt, confout, edr, mdlog]
                                   $ words "traj.trr traj.xtc state.cpt confout.gro ener.edr md.log"
                  in runMdrunFileChecker $ do
                                forM_ checks $ \(f, n) -> do
                                             let path = w </> n
                                             e <- liftIO $ doesFileExist path
                                             when e $ 
                                                  f .= (Just path)


mdrun :: Exe (Result Mdrun) -> Exe (Result Mdrun)
mdrun e = do
  downWorkarea "mdrun"
  exe "mdrun"
  e

threads :: Int -> Exe ()
threads n = flags ["-nt", show n]

topol :: FilePath -> Exe ()
topol p = flags ["-s", p]


test :: Exe (Result Mdrun)
test = do
  workarea "/tmp/sqew_wa"
  r0 <- prog $ pdb2gmx $ do
          exe "/opt/gromacs/4.5.5-static/bin/pdb2gmx"
          cnf <- liftIO $ (</> "testfiles/conf.gro") <$> getCurrentDirectory
          Pdb2Gmx.struct cnf
          Pdb2Gmx.ff Amber96
          Pdb2Gmx.water NoWater
          Pdb2Gmx.ignh
          run

  r1 <- prog $ grompp $ do
          let Just o = output r0
          exe "/opt/gromacs/4.5.5-static/bin/grompp"
          Grompp.struct $ Pdb2Gmx.conf o
          Grompp.topol  $ Pdb2Gmx.topol o
          mdp    $ do
            load "testfiles/sim.mdp"
            entry $ newEntry "nsteps" "1000"
            
          run

  r3 <- prog $ mdrun $ do
          let Just o = output r1
          exe "/opt/gromacs/4.5.5-static/bin/mdrun"
          topol $ Grompp.tpr o
          run


  return r3


t = runExe test
