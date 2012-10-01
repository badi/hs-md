{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Science.MD.GMX.Internal.Executable.Pdb2Gmx where

import Science.MD.GMX.Internal.ExecutableBuilder hiding (test, t)

import Control.Lens

import System.FilePath

import "mtl" Control.Monad.State



data ForceField = Amber96
                | Amber99
                | Amber99sb
                | Amber99sb_ildn_nmr
                | Amber03
                  deriving (Eq, Show)

instance ToFlags ForceField where
    toFlags f = ["-ff", f']
        where f' = case f of
                     Amber96            -> "amber96"
                     Amber99            -> "amber99"
                     Amber99sb          -> "amber99sb"
                     Amber99sb_ildn_nmr -> "amber99sb-ildn-nmr"
                     Amber03            -> "amber03"


data Water = NoWater | SPC | Tip3p | Tip4p
           deriving (Eq, Show)

instance ToFlags Water where
    toFlags w = ["-water", w']
        where w' = case w of
                     NoWater -> "none"
                     SPC     -> "spc"
                     Tip3p   -> "tip3p"
                     Tip4p   -> "tip4p"

data Vsite = NoVsite | Hydrogens | Aromatics
           deriving (Eq, Show)

instance ToFlags Vsite where
    toFlags v = ["-vsite", v']
        where v' = case v of
                     NoVsite   -> "none"
                     Hydrogens -> "hydrogens"
                     Aromatics -> "aromatics"

data Output = MkOutput {
      conf  :: FilePath
    , topol :: FilePath
    , posre :: FilePath
    } deriving Show

instance GetOutputFiles Output where
    getOutput s = MkOutput c t p
        where [c,t,p] = map (s ^. exeWorkarea </>) ["conf.gro", "topol.top", "posre.itp"]

    

pdb2gmx :: Exe a -> Exe a
pdb2gmx e = do
  myState <- get
  downWorkarea "pdb2gmx"
  exe "pdb2gmx"
  exe "/opt/gromacs/4.5.5-static/bin/pdb2gmx"
  result <- e
  put myState
  return result


ff :: ForceField -> Exe ()
ff = flags . toFlags

water :: Water -> Exe ()
water = flags . toFlags

vsite :: Vsite -> Exe ()
vsite = flags . toFlags

struct :: FilePath -> Exe ()
struct p = flags ["-f", p]

ignh :: Exe ()
ignh = flag "-ignh"



-- -------------------------------------------------------------------------------- --

test :: Exe (Result Output)
test = do
  workarea "/tmp/sqew_wa"
  pdb2gmx $ do
         struct "/tmp/test.pdb"
         ff Amber96
         water NoWater
         ignh
         run

t = runExe $ do r <- test
                liftIO $ print $ exitcode r
                liftIO $ print $ output r

