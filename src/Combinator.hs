module Combinator where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Lambda

type Env = Map String Lambda

newEnv :: Env
newEnv = Map.empty

-- | add bindings to global variables
addToEnv :: Lang -> Env -> Either String Env
addToEnv (Named name body) env = if Map.member name env
    then Left $ "Error: trying to redefine " <> name
    else Right $ Map.insert name body env
addToEnv _ env = Right env

-- | rebind local variable by deleting the global (upper one)
rebindLocal :: String -> Env -> Env
rebindLocal = Map.delete

rebindApp :: String -> Lambda -> Env -> Env
rebindApp = Map.insert

clearEnv :: Env -> Env
clearEnv = const Map.empty

reduce :: Env -> Lambda -> Lambda
reduce env (Atom a) = case Map.lookup a env of
    Just expr -> reduce env expr
    Nothing   -> Atom a
reduce env (Fun x           b   ) = Fun x (reduce (rebindLocal x env) b)
reduce env (App a@(Atom _ ) expr) = reduce env $ App (reduce env a) expr
reduce env (App (  Fun x b) expr) = reduce (rebindApp x expr env) b
reduce _   _                      = undefined

-- tests

testEnv :: Either String Env
testEnv = addToEnv (Named "id" (Fun "x" (Atom "x"))) newEnv
