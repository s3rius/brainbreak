module Compiler.Backends.C.AST where

data CType
  = CTypeInt
  | CTypeMap CType CType
  deriving (Eq, Show)

data CVar
  = CVar String CType
  | MapElement CVar CVar
  deriving (Eq, Show)

data CConst
  = CConstString String
  | CConstInt Int
  | CEmptyMap CType CType
  deriving (Eq, Show)

data CValue
  = CValue CVar
  | CValueConst CConst
  deriving (Show, Eq)

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
