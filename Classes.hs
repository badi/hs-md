

type Temperature = Double

data FF = Amber96
        | Amber99SB
        | Amber99SBILDN
        | Amber99SBILDNNMR
          deriving Show

data Timestep = FS1 | FS2 | FS5 | Custom Double deriving Show

data Water = TIP3P | TIP4P | GB_OBC deriving Show

data MDType = GMX | NAMD deriving Show


newtype Command = MkCmd String

class Commandline a where
    command :: a -> [Command]


class Config a where
    structure :: a -> FilePath
    temperature :: a -> Temperature
    timestep :: a -> Timestep
    forcefield :: a -> FF
    watermodel :: a -> Water