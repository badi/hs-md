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


import System.Directory
import System.Process
import System.Exit


type Flag = String


class ToCommand a where
    type Command
    toCommand :: a -> Command



data ExeState = MkExeState {
      _exeWorkarea :: FilePath
    , _exeName :: String
    , _exeFlags :: [Flag]
    } deriving Show

makeLenses ''ExeState

emptyState :: ExeState
emptyState = MkExeState "." "<some exe>" []

instance ToCommand ExeState where
    type Command = String
    toCommand s = intercalate ";" cmds
        where cmds = [ "pushd " ++ view exeWorkarea s
                     , cmd
                     , "popd" 
                     ]
              cmd = view exeName s ++ " " ++ flags
              flags = unwords $ view exeFlags s


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

exe :: String -> Exe ()
exe n = exeName .= n

flag :: Flag -> Exe ()
flag f = exeFlags ++= [f]


run :: Exe ExitCode
run = do
  cmd <- toCommand <$> get
  liftIO $ putStrLn $ "Executing command: " ++ cmd
  liftIO $ system cmd

test = do
  workarea "/tmp/sqew_wa"
  exe "echo"
  flag "hello"
  flag "world"
  run


t = runExe test
