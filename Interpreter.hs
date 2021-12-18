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

data Value = N Integer | C Closure
newtype Closure = Closure Exp Env

data Env = Env {
  vars :: [Map Id Value],
  funs :: Map Id Value
}

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Error monad.

type Err = Except String

eval :: Exp -> State Env Value
eval exp = case exp of
  EVar id -> do
    let v = lookup id
    case v of
      N i -> return i
      C (Closure e gamma) -> do
        update env gamma
        eval e
  EInt e -> return e
  EApp e ->
  EAdd e1 e2 -> do
    let e1' = eval e1
    let e2' = eval e2
    return e1' + e2'
  ESub e1 e2 -> do
    let e1' = eval e1
    let e2' = eval e2
    return e1' - e2'
  ELt e1 e2 -> do
    let e1' = eval e1
    let e2' = eval e2
    if e1' < e2' then return 1 else return 0
  EIf e1 e2 e3 -> do
    let e1' = eval e1
    if e1' == 1 then eval e2 
      else eval e3
  EAbs id e -> return Closure e 



-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  throwError "TODO: implement interpreter"
