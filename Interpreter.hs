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

import Fun.Abs
import Fun.Print
import Control.Monad.State
import qualified Data.List as List
import Prelude hiding (lookup)

data Closure = Closure (Exp, Env)
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
-- eval env exp s | trace (show exp ++ "\n" ++ show (vars env) ++ "\n\n") False = undefined
eval env exp s = case exp of
  
  EVar id -> do
    let Closure (exp', gamma) = lookup id env
    eval Env{vars = vars gamma, funs = funs env} exp' s
  
  EInt i -> return $ Closure (EInt i, emptyEnv)
  
  EApp id e -> do
    closure <- eval env id s
    case closure of 
      Closure (EAbs id' exp', delta) -> do
        if s == CallByValue then do
          u <- eval env e s
          eval (updateVar Env{vars = vars delta, funs = funs env} id' u) exp' s
        else do
          let u = Closure (e, Env{vars = vars env, funs = Map.empty})
          eval (updateVar Env{vars = vars delta, funs = funs env} id' u) exp' s
      _ -> error "Invalid function application"

  EAdd e1 e2 -> do
    c1 <- eval env e1 s 
    let Closure (e1', _) = c1
    c2 <- eval env e2 s 
    let Closure (e2', _) = c2
    case (e1', e2') of
      (EInt i, EInt j) -> return $ Closure (EInt (i+j), emptyEnv)
      _ -> error "Addition is only defined for integers"
    
  ESub e1 e2 -> do
    c1 <- eval env e1 s 
    let Closure (e1', _) = c1
    c2 <- eval env e2 s
    let Closure (e2', _) = c2
    case (e1', e2') of
      (EInt i, EInt j) -> return $ Closure (EInt (i-j), emptyEnv)
      _ -> error "Subtraction is only defined for integers"
  
  ELt e1 e2 -> do
    e1'' <- eval env e1 s
    let Closure (e1', _) = e1''
    e2'' <- eval env e2 s
    let Closure (e2', _) = e2''
    case (e1', e2') of
      (EInt i, EInt j) | i < j -> return $ Closure (EInt 1, emptyEnv)
      (EInt i, EInt j) -> return $ Closure (EInt 0, emptyEnv)
      _ -> error "Less than operator is only defined for integers"
  
  EIf e1 e2 e3 -> do
    e1'' <- eval env e1 s
    let Closure (e1', _) = e1''
    case e1' of 
      EInt 0 -> eval env e3 s
      EInt 1 -> eval env e2 s
      _ -> error "If/then/else statement is only defined for booleans"
  
  EAbs id e -> return $ Closure (exp, Env{vars = vars env, funs = Map.empty})

lookup :: Ident -> Env -> Closure
lookup id@(Ident name) env = case Map.lookup id (vars env) of
    Just closure -> closure
    Nothing -> case Map.lookup id (funs env) of
      Nothing -> error $ "Unknown variable: " ++ name
      Just closure -> closure

updateFun :: Env -> Ident -> Closure -> Env
updateFun env id val = Env{vars = vars env, funs = Map.insert id val (funs env)}

updateVar :: Env -> Ident -> Closure -> Env
updateVar env id val = Env{vars = Map.insert id val (vars env), funs = funs env}

emptyEnv :: Env
emptyEnv = Env{vars = Map.empty, funs = Map.empty}

updateDefs :: [Def] -> Env -> Env
updateDefs [] env = env
updateDefs (def@(DDef id ids exp):defs) env = do
  let closure = makeClosure def
  let env' = updateFun env id closure
  updateDefs defs env'

makeClosure :: Def -> Closure
makeClosure (DDef name [] exp) = Closure (exp, emptyEnv)
makeClosure (DDef name (id:ids) exp) = do
  let Closure (exp', env') = makeClosure (DDef name ids exp)
  let exp = EAbs id exp'
  Closure (exp, env')

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  let env = updateDefs defs emptyEnv
  closure <- eval env mainExp strategy
  case closure of
    Closure (EInt i, _) -> return i
    _ -> error "Main must evaluate to integer"

