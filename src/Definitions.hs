module Definitions where

data BrainBreakOperation = Increment 
    | Decrement
    | MoveRight
    | MoveLeft
    | Read
    | Write
    | Comment
    | Loop BrainBreakBlock
    deriving (Eq, Show)

type BrainBreakBlock = [BrainBreakOperation]