-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

{-# LANGUAGE LambdaCase #-}

module Interpreter (interpret, Strategy(..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace


import Fun.Abs
import Fun.Print
import Control.Monad.State
import qualified Data.List as List
import Prelude hiding (lookup)

--data Value = N Integer | C Closure
newtype Closure = Closure (Exp, Env)
  deriving Show

data Env = Env {
  vars :: Map Ident Closure,
  funs :: Map Ident Closure
}
  deriving Show

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue
  deriving Eq

-- | Error monad.

type Err = Except String

eval :: Env -> Exp -> Strategy -> Err Closure
--eval env exp s | trace (show exp ++ "\n" ++ show (vars env) ++ "\n\n") False = undefined
eval env exp s = case exp of
  EVar id -> do
    let v@(Closure (exp', gamma)) = lookup id env
    let env' = Env{vars = vars gamma, funs = funs env}
    eval env' exp' s
  EInt i -> return $ Closure (EInt i, emptyEnv)
  EApp id e -> do
    v <- eval env id s
    let Closure (b, delta) = v
    let env' = Env{vars = vars delta, funs = funs env} 
    let x = head $ Map.keys (vars delta)
    if s == CallByValue then do
      u <- eval env e s
      let env'' = updateVar env' x u
      eval env'' b s 
    else do
      let u = Closure (e, Env{vars = vars env, funs = Map.empty})
      let env'' = updateVar env' x u 
      eval env'' b s 
    -- (\x.b){delta} = eval(gamma id)
    --eval(update((functions(gamma), delta), x, u), b)
    --env' <- updateFun
  EAdd e1 e2 -> do
    c1 <- eval env e1 s 
    let Closure (EInt e1', _) = c1
    c2 <- eval env e2 s 
    let Closure (EInt e2', _) = c2
    return $ Closure (EInt (e1' + e2'), emptyEnv)
  ESub e1 e2 -> do
    c1 <- eval env e1 s 
    let Closure (EInt e1', _) = c1
    c2 <- eval env e2 s
    let Closure (EInt e2', _) = c2
    return $ Closure (EInt (e1' - e2'), emptyEnv)
  ELt e1 e2 -> do
    Closure (e1', _) <- eval env e1 s
    Closure (e2', _) <- eval env e2 s
    if e1' < e2' then return $ Closure (EInt 1, emptyEnv)  else return $ Closure (EInt 0, emptyEnv)
  EIf e1 e2 e3 -> do
    Closure (e1', _) <- eval env e1 s 
    if e1' == EInt 1 then eval env e2 s
      else eval env e3 s
  EAbs id e -> return $ Closure (exp, Env{vars = vars env, funs = Map.empty})-- eval  e s

lookup :: Ident -> Env -> Closure
lookup id env = case Map.lookup id (vars env) of
    Just closure -> closure
    Nothing -> case Map.lookup id (funs env) of
      Nothing -> error $ "lookup " ++ show id
      Just closure -> closure

updateFun :: Env -> Ident -> Closure -> Env
updateFun env id val = Env{vars = vars env, funs = Map.insert id val (funs env)}

updateVar :: Env -> Ident -> Closure -> Env
updateVar env id cl | trace (show id ++ "\n") False = undefined
updateVar env id val = Env{vars = Map.insert id val (vars env), funs = funs env}

emptyEnv :: Env
emptyEnv = Env{vars = Map.empty, funs = Map.empty}

updateDefs :: [Def] -> Env -> Env
updateDefs [] env = env
updateDefs (def@(DDef id ids exp):defs) env = do
  let closure = Closure (makeLambda def, emptyEnv)
  let env' = updateFun env id closure
  updateDefs defs env'

makeLambda :: Def -> Exp
makeLambda (DDef name [] exp) = (EAbs (Ident "dummyID") exp)
makeLambda (DDef name (id:ids) exp) = 
  (EAbs id (makeLambda (DDef name ids exp)))

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  let env = updateDefs defs emptyEnv
  closure <- eval env mainExp strategy
  error $ "closure " ++ show closure
  let Closure (EInt i, _) = closure
  return i
