{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}


{- A braindump for what a DSL to create MD simulations might look like -}

import Control.Lens
import Control.Monad.State


type Temperature = Double

data FF = Amber96
        | Amber99SB
        | Amber99SBILDN
        | Amber99SBILDNNMR
          deriving Show

data Timestep = FS1 | FS2 | FS5 | Custom Double deriving Show

data Water = TIP3P | TIP4P | GB_OBC deriving Show

data MD = GMX | NAMD deriving Show

data SQS = SQS {
      _structures     :: [FilePath]
    , _temperatures   :: [Temperature]
    , _forcefields    :: [FF]
    , _timesteps      :: [Timestep]
    , _watermodels    :: [Water]
    , _mdsoftwares    :: [MD]
    , _outputlocation :: FilePath
    } deriving Show

makeLenses ''SQS

newSQS :: SQS
newSQS = SQS {
           _structures     = []
         , _temperatures   = []
         , _forcefields    = []
         , _timesteps      = []
         , _watermodels    = []
         , _mdsoftwares    = []
         , _outputlocation = "."
         }


newtype SQ a = MkSQ {
      unSQ :: State SQS a
    } deriving (Functor, Monad, MonadState SQS)


data Config = Cfg {
      _structure :: FilePath
    , _temperature :: Temperature
    , _forcefields :: FF
    , _timestep :: Timestep
    , _watermodel :: Water
    , _mdsoftware



pushSQS :: MonadState s m => Setting s s [a] [a] -> a -> m ()
pushSQS f v = f %= (v:)

setSQS :: MonadState a m => Setting a a c d -> d -> m ()
setSQS f v = f .= v



struct :: FilePath -> SQ ()
struct = pushSQS structures

temp :: Temperature -> SQ ()
temp = pushSQS temperatures

ff :: FF -> SQ ()
ff = pushSQS forcefields

dt :: Timestep -> SQ ()
dt = pushSQS timesteps

water :: Water -> SQ ()
water = pushSQS watermodels

md :: MD -> SQ ()
md = pushSQS mdsoftwares

output :: FilePath -> SQ ()
output = setSQS outputlocation


runSQ :: SQ a -> (a, SQS)
runSQ a = runState (unSQ a) newSQS


test :: SQ ()
test = do
  mapM_ struct (words "ww0 ww1 ww2")
  mapM_ temp   [300, 370, 395]
  mapM_ ff     [Amber96]
  mapM_ water  [GB_OBC]
  md GMX
  output "/data/sqewtest"

main :: IO ()
main = print . snd $ runSQ test
