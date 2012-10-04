{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Science.MD.GMX.Internal.Executable.Grompp where

import Science.MD.GMX.Internal.ExecutableBuilder hiding (test, t)
import Science.MD.GMX.Internal.Executable.Pdb2Gmx (Pdb2Gmx, ForceField(..), Water(..), pdb2gmx)
import qualified Science.MD.GMX.Internal.Executable.Pdb2Gmx as Pdb2Gmx

import Science.MD.GMX.Internal.MDP
import Science.MD.GMX.Internal.MDPBuilder hiding (t)

import Control.Applicative ((<$>))

import Control.Lens

import "mtl" Control.Monad.State
import Data.Maybe (fromJust)
import System.FilePath
import System.Directory


data Grompp = MkGrompp {
      tpr :: FilePath
    } deriving Show

instance GetOutputFiles Grompp where
    getOutput s = MkGrompp $ s ^. cmdWorkarea </> "topol.tpr"


grompp :: Exe (Result Grompp) -> Exe (Result Grompp)
grompp e = do
  downWorkarea "grompp"
  exe "grompp"
  e
  

mdp :: MDP a -> Exe ()
mdp m = do
  mdpName <- (</> "grompp.mdp") . view (exeCurrentCommand . cmdWorkarea) <$> get
  cfg <- liftIO $ toMDP . snd <$> runMDP m
  liftIO $ writeFile mdpName cfg
  liftIO $ putStrLn $ "Writing mdp to " ++ mdpName
  flags ["-f", mdpName]


struct :: FilePath -> Exe ()
struct c = flags ["-c", c]

topol :: FilePath -> Exe ()
topol t = flags ["-p", t]


test :: Exe (Result Grompp)
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
          exe "/opt/gromacs/4.5.5-static/bin/grompp"
          let Just o = output r0
          struct $ Pdb2Gmx.conf o
          topol  $ Pdb2Gmx.topol o
          mdp    $ load "testfiles/sim.mdp"
          run


  return r1


t = runExe test
