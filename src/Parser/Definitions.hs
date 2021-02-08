module Parser.Definitions where

data BrainBreakOperation
  = Increment
  | Decrement
  | MoveRight
  | MoveLeft
  | Read
  | Write
  | Comment
  | Loop BrainBreakBlock
  deriving (Eq, Show)

type BrainBreakBlock = [BrainBreakOperation]

data REPLHelpers
  = PrintState
  | PrintBuf
  | PrintBufChars
  deriving (Show, Eq)

data REPLCode
  = Code BrainBreakBlock
  | Helper REPLHelpers
  deriving (Show, Eq)
