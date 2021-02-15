module Compiler.Backends.C.Builder where

import           Compiler.Backends.C.AST
import           Interpreter.Definitions

import           Control.Monad.State

data BuilderState =
  BuilderState
    { _buffer  :: CVar
    , _counter :: CVar
    }
  deriving (Show)

mapInstructions :: InterpreterCode -> State BuilderState COperation
mapInstructions (InterAdd value) = do
  buffer <- gets _buffer
  counter <- gets _counter
  return $
    CAdd
      (MapElement buffer counter)
      (MapElement buffer counter)
      (CValueConst (CConstInt value))
mapInstructions (InterMov value) = do
  counter <- gets _counter
  return $ CAdd counter counter (CValueConst (CConstInt value))
mapInstructions (InterSet value) = do
  counter <- gets _counter
  buffer <- gets _buffer
  return $ CSet (MapElement buffer counter) (CValueConst (CConstInt value))
mapInstructions InterRead = do
  counter <- gets _counter
  buffer <- gets _buffer
  return $ CRead (MapElement buffer counter)
mapInstructions InterWrite = do
  counter <- gets _counter
  buffer <- gets _buffer
  return $ CPrint (MapElement buffer counter)
mapInstructions (InterLoop code) = do
  exprs <- mapM mapInstructions code
  buffer <- gets _buffer
  counter <- gets _counter
  return $ CLoop (MapElement buffer counter) exprs

parseInterpreterCode :: InterpreterCodeBlock -> State BuilderState [COperation]
parseInterpreterCode code = do
  state <- get
  mapM mapInstructions code

buildModule :: InterpreterCodeBlock -> CModule
buildModule intrepreterCode = do
  let counter = CVar "counter" CTypeInt
  let buffer = CVar "buffer" (CTypeMap CTypeInt CTypeInt)
  let initialState = BuilderState {_buffer = buffer, _counter = counter}
  let operations = evalState (parseInterpreterCode intrepreterCode) initialState
  let c_module =
        CModule
          [CInclude "cstdio", CInclude "unordered_map", CUsingNamespace "std"]
          ([ CDeclare counter
           , CDeclare buffer
           , CSet counter (CValueConst (CConstInt 0))
           , CSet buffer (CValueConst (CEmptyMap CTypeInt CTypeInt))
           ] ++
           operations)
  c_module
