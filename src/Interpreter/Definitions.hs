module Interpreter.Definitions where

data InterpreterCode
  = InterAdd Int
  | InterMov Int
  | InterSet Int
  | InterRead
  | InterWrite
  | InterLoop
      { _block :: InterpreterCodeBlock
      }
  deriving (Eq, Show)

type InterpreterCodeBlock = [InterpreterCode]
