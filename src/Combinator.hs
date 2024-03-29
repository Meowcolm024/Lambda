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

-- | unfold binding in the env
--   reject recursive bindings and bindings with free variables
unfold :: Maybe String -> Env -> Lambda -> Either String Lambda
unfold (Just x) env (Atom a) = if x == a
    then Left $ "Error: recursive definition of " <> x
    else case Map.lookup a env of
        Just expr ->
            if expr == Atom a then Right expr else unfold Nothing env expr      -- assume well-formed expr in env
        Nothing -> Left $ "Error: free variables in definition of " <> x
unfold Nothing env (Atom a) = case Map.lookup a env of
    Just expr -> if expr == Atom a then Right expr else unfold Nothing env expr
    Nothing   -> Right $ Atom a
unfold (Just x) env (Fun v b) = Fun v <$> if x == v
    then unfold Nothing (rebindApp v (Atom v) env) b
    else unfold (Just x) (rebindApp v (Atom v) env) b
unfold x env (Fun v b) = Fun v <$> unfold x (rebindApp v (Atom v) env) b
unfold x env (App l r) = App <$> unfold x env l <*> unfold x env r

-- | check whether an expression is irreducible
irreducible :: Env -> Lambda -> Bool
irreducible env (Atom a) = case Map.lookup a env of
    Just expr -> expr == Atom a
    Nothing   -> True
irreducible _   (App (Fun _ _) _) = False
irreducible env (App l         r) = irreducible env l && irreducible env r
irreducible env (Fun x         b) = irreducible (rebindLocal x env) b

{-# WARNING reduce "Incorrect code" #-}
reduce :: Env -> Lambda -> Lambda
reduce _   (Atom a ) = Atom a
reduce env (Fun x b) = Fun x (reduce' (rebindLocal x env) b)    -- ! error here ! this is eager
reduce env (App a@(Atom _) r@(Atom _)) =
    reduce' env $ App (reduce' env a) (reduce' env r)
reduce env (App a@(Atom _ ) expr) = reduce' env $ App (reduce' env a) expr
reduce env (App (  Fun x b) expr) = reduce' (rebindApp x expr env) b
reduce env (App l           r   ) = reduce' env $ App (reduce' env l) r

reduce' :: Env -> Lambda -> Lambda
reduce' env expr = if irreducible env expr then expr else reduce env expr
