{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Science.MD.GMX.Internal.Executable.Pdb2Gmx where

import Science.MD.GMX.Internal.ExecutableBuilder hiding (test, t)

import Control.Lens

import System.FilePath



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


pdb2gmx :: Exe ()
pdb2gmx = do
  subWorkarea "pdb2gmx"
  exe "pdb2gmx"
  exe "/opt/gromacs/4.5.5-static/bin/pdb2gmx"


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


test = do
  workarea "/tmp/sqew_wa"
  pdb2gmx
  struct "/tmp/test.pdb"
  ff Amber96
  water NoWater
  ignh
  run

t = runExe test

