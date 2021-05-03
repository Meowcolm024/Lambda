module Combinator where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Lambda

type Env = Map String Lambda

newEnv :: Env
newEnv = Map.empty

addToEnv :: Lang -> Env -> Either String Env
addToEnv (Named name body) env = if Map.member name env
    then Left $ "Error: trying to redefine " <> name
    else Right $ Map.insert name body env
addToEnv _ env = Right env

clearEnv :: Env -> Env
clearEnv = const Map.empty
