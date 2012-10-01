{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Science.MD.GMX.Internal.ExecutableBuilder where

import Control.Applicative ((<$>))

import Control.Lens
import Data.List.Lens

import "mtl" Control.Monad.State

import Data.List (intercalate)

import System.FilePath ((</>))
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



data ExeState = MkExeState {
      _exeWorkarea :: FilePath
    , _exeName :: String
    , _exeFlags :: [Flag]
    } deriving Show

makeLenses ''ExeState

emptyState :: ExeState
emptyState = MkExeState "." "<some exe>" []

instance ToCommand ExeState where
    toCommand s = c
        where p = proc (s ^. exeName) (s ^. exeFlags)
              c = p { cwd = Just $ s ^. exeWorkarea
                    , env = Nothing
                    , std_in = Inherit
                    , std_out = Inherit
                    , std_err = Inherit
                    }



newtype Exe a = MkExe {
      unExe :: StateT ExeState IO a
    } deriving (Functor, Monad, MonadIO, MonadState ExeState)


runExe :: Exe a -> IO (a, ExeState)
runExe = flip runStateT s0 . unExe
    where s0 = emptyState



workarea :: FilePath -> Exe ()
workarea p = do
  e <- liftIO $ doesDirectoryExist p
  when (not e) $ liftIO $ do
               liftIO $ putStrLn $ "WARNING: creating directory " ++ p
               createDirectory p
  exeWorkarea .= p

subWorkarea :: String -> Exe ()
subWorkarea n = do
  wa <- view exeWorkarea <$> get
  let wa' = wa </> n
  workarea wa'

exe :: String -> Exe ()
exe n = exeName .= n

flag :: Flag -> Exe ()
flag f = flags [f]

flags :: [Flag] -> Exe ()
flags f = exeFlags ++= f


run :: Exe ExitCode
run = do
  cmd <- toCommand <$> get
  liftIO $ putStrLn $ "Executing command: " ++ show cmd
  (stdinh, stdouth, stderrh, ph) <- liftIO $ createProcess cmd
  liftIO $ waitForProcess ph

test = do
  workarea "/tmp/sqew_wa"
  exe "echo"
  flag "hello"
  flag "world"
  run


t = runExe test
