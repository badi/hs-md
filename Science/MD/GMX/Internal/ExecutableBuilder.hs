{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Science.MD.GMX.Internal.ExecutableBuilder where

import Control.Applicative ((<$>))

import Control.Lens
import Data.List.Lens

import "mtl" Control.Monad.State

import Data.List (intercalate)
import Data.Maybe

import System.FilePath
import System.Directory
import System.Process
import System.Exit


type Flag = String


class ToCommand a where
    type Command
    type Command = CreateProcess
    toCommand :: a -> Command

class ToFlags a where
    toFlags :: a -> [Flag]

instance Show CreateProcess where
    show p = cmd
        where cmd = show $ cmdspec p

instance Show CmdSpec where
    show (ShellCommand s) = s
    show (RawCommand e args) = e ++ " " ++ intercalate " " args

class GetOutputFiles a where
    getOutput :: CommandState -> a


data Result a where
    MkResult :: GetOutputFiles a => ExitCode -> Maybe a -> Result a

deriving  instance Show a => Show (Result a)

exitcode :: Result a -> ExitCode
exitcode (MkResult e _) = e

output :: GetOutputFiles a => Result a -> Maybe a
output (MkResult _ o) = o


data CommandState = MkCommandState {
      _cmdWorkarea :: FilePath
    , _cmdName :: Maybe String
    , _cmdFlags :: [Flag]
    } deriving Show

makeLenses ''CommandState

emptyCommand :: CommandState
emptyCommand = MkCommandState "." Nothing []

instance ToCommand CommandState where
    toCommand s = c
        where p = proc n (s ^. cmdFlags)
              n = maybe (error $ "Executable is not specified in " ++ show s)
                        id
                        (s ^. cmdName)
              c = p { cwd = Just $ s ^. cmdWorkarea
                    , env = Nothing
                    , std_in = Inherit
                    , std_out = Inherit
                    , std_err = Inherit
                    }

data ExeState = MkExeState {
      _exeCurrentCommand :: CommandState
    , _exeHistory :: [CommandState]
    } deriving Show

makeLenses ''ExeState

emptyState :: ExeState
emptyState = MkExeState emptyCommand []

newtype Exe a = MkExe {
      unExe :: StateT ExeState IO a
    } deriving (Functor, Monad, MonadIO, MonadState ExeState)


runExe :: Exe a -> IO (a, ExeState)
runExe = flip runStateT s0 . unExe
    where s0 = emptyState


prog :: GetOutputFiles a => Exe (Result a) -> Exe (Result a)
prog e = do
  r <- e
  c <- _exeCurrentCommand <$> get
  exeHistory %= (c:)
  exeCurrentCommand .= emptyCommand { _cmdWorkarea = c ^. cmdWorkarea }
  return r



workarea :: FilePath -> Exe ()
workarea p = do
  e <- liftIO $ doesDirectoryExist p
  when (not e) $ liftIO $ do
               liftIO $ putStrLn $ "WARNING: creating directory " ++ p
               createDirectory p
  exeCurrentCommand . cmdWorkarea .= p

downWorkarea :: String -> Exe ()
downWorkarea n = do
  wa <- view (exeCurrentCommand . cmdWorkarea) <$> get
  let wa' = wa </> n
  workarea wa'

upWorkarea :: Exe ()
upWorkarea = (exeCurrentCommand . cmdWorkarea) %= takeDirectory

cwd :: Exe FilePath
cwd = liftIO getCurrentDirectory

exe :: String -> Exe ()
exe n = (exeCurrentCommand . cmdName) .= Just n

flags :: [Flag] -> Exe ()
flags f = (exeCurrentCommand . cmdFlags) ++= f

flag :: Flag -> Exe ()
flag f = flags [f]


run :: GetOutputFiles a => Exe (Result a)
run = do
  cmd <- toCommand . view exeCurrentCommand <$> get
  liftIO $ putStrLn $ "Executing command: " ++ show cmd
  (stdinh, stdouth, stderrh, ph) <- liftIO $ createProcess cmd
  ecode <- liftIO $ waitForProcess ph
  s <- view exeCurrentCommand <$> get
  return $ case ecode of
             ExitSuccess -> MkResult ecode (Just $ getOutput s)
             _           -> MkResult ecode Nothing
  



-- -------------------------------------------------------------------------------- --

data IdentityResult = IR deriving Show

instance GetOutputFiles IdentityResult where getOutput = const IR

test :: Exe (Result IdentityResult)
test = prog $ do
  workarea "/tmp/sqew_wa"
  exe "echo"
  flag "hello"
  flag "world"
  run


t = runExe test
