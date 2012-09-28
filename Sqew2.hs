{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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

data MDType = GMX | NAMD deriving Show


class SQConfig t where
    type Config :: *

    -- new :: Config t
    -- setProperty :: Config t -> a -> Config t

    -- setStructure :: Config t -> FilePath -> Config t
    -- setStructure = setProperty

    -- setTemperature :: Config t -> Temperature -> Config t
    -- setTemperature = setProperty

    -- setForceField :: Config t -> FF -> Config t
    -- setForceField = setProperty

    -- setWaterModel :: Config t -> Water -> Config t
    -- setWaterModel = setProperty

    -- setOutputLocation :: Config t -> FilePath -> Config t
    -- setOutputLocation = setProperty



data Foo

instance SQConfig Foo where
    type Config Foo = Foo

-- data Bar
-- instance SQConfig Bar where
--     data Config Bar = DataBar Int Double deriving Show

-- t :: Config Foo
-- t = DataFoo 42 24


-- data SQS a = MkSQS {
--       _mdtype :: MDType
--     , _config :: a
--     }

--     -- , _structure :: FilePath
--     -- , _temperature :: Maybe Temperature
--     -- , _forcefield :: Maybe FF
--     -- , _timestep :: Maybe Timestep
--     -- , _watermodel :: Maybe Water
--     -- , _outputlocation :: Maybe FilePath
--     -- }

-- makeLenses ''SQS


