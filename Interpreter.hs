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

data Value = Integer |  (Exp, Env)

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

eval :: Exp -> State Env Int



-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  throwError "TODO: implement interpreter"