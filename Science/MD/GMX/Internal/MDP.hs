{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Science.MD.GMX.Internal.MDP where

import Control.Lens hiding (value)

import Data.Map (Map)
import qualified Data.Map as Map



type MDPKey = String
type MDPVal = String
data MDPEntry = MkMDPEntry {
      _eKey :: MDPKey
    , _eVal :: MDPVal
    } deriving Show

makeLenses ''MDPEntry

newEntry :: MDPKey -> MDPVal -> MDPEntry
newEntry = MkMDPEntry


data MDPData = MkMDPData {
      _mdpData :: Map MDPKey MDPVal
    } deriving Show

makeLenses ''MDPData

emptyData :: MDPData
emptyData = MkMDPData Map.empty

addEntry :: MDPEntry -> MDPData -> MDPData
addEntry e d = d { _mdpData = Map.insert (view eKey e) (view eVal e) (view mdpData d) }

hasKey :: MDPKey -> MDPData -> Bool
hasKey k d = Map.member k (view mdpData d)

getKey :: MDPKey -> MDPData -> Maybe MDPVal
getKey k d = Map.lookup k (view mdpData d)

class ToMDP a where
    toMDP :: a -> String

instance ToMDP MDPEntry where
    toMDP e = view eKey e ++ " = " ++ view eVal e

instance ToMDP (Map MDPKey MDPVal) where
    toMDP = unlines . reverse . Map.foldlWithKey f []
        where f :: [String] -> MDPKey -> MDPVal -> [String]
              f acc k v = (k ++ " = " ++ v) : acc

instance ToMDP MDPData where
    toMDP = toMDP . view mdpData


