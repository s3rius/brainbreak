module Definitions where

data BrainBreakOperation = Increment 
    | Decrement
    | MoveRight
    | MoveLeft
    | Read
    | Write
    | Loop BrainBreakBlock
    deriving (Eq, Show)

type BrainBreakBlock = [BrainBreakOperation]