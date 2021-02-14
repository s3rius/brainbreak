module Compiler.Backends.C.AST where

import           Control.Lens

data CType
  = CTypeInt
  | CTypeMap CType CType
  deriving (Eq, Show)

data CVar
  = CVar
      { _name :: String
      , _type :: CType
      }
  | MapElement CVar CVar
  deriving (Eq, Show)

data CConst
  = CConstString String
  | CConstInt Int
  | CEmptyMap CType CType
  deriving (Eq, Show)

type CValue = Either CVar CConst

data COperation
  = CPrint CVar
  | CRead CVar
  | CDeclare CVar
  | CSet CVar CValue
  | CAdd CVar CVar CValue
  | CDecrease CVar CVar CValue
  | CLoop CVar [COperation]
  deriving (Eq, Show)

data CHeader
  = CInclude String
  | CUsingNamespace String
  deriving (Eq, Show)

data CModule =
  CModule [CHeader] [COperation]
  deriving (Eq, Show)