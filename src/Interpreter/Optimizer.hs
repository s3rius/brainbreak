module Interpreter.Optimizer
  ( optimize
  , optimizer
  , preprocess
  ) where

import           Interpreter.Definitions
import           Parser.Definitions
import           Parser.Parser

{-
Optiomization plans:
 * Instruction merging
 * Efficient cell zeroing
 * Dead code elimination
 * Instruction conversion

Insstruction merging
 Precalculate the result of operation brefore compile/During interpreting.
    +++ --- ++ => ++
    +++ --- --- => ---
    >>> << => >
    > <<<< => <<<<
    +++ --- => Deletes operation

Efficient cell zeroing
    [-] -> Set current cell to zero

Dead code elimination
    Because the next cell is probably zero, delete the second loop.
    Maybe this check is unnecesary.
    [+>][<-] => [+>]
    Also remove Loops at the begining of the programm.

Instruction conversion
    Just for efficency
    [-]+++ => Set 0, Add 3 => Set 3
    +++[-] => Set 0
    Also sequential Set instructions may be merged.
-}
mapCodes :: BrainBreakOperation -> InterpreterCode
mapCodes Increment          = InterAdd 1
mapCodes Decrement          = InterAdd (-1)
mapCodes MoveRight          = InterMov 1
mapCodes MoveLeft           = InterMov (-1)
mapCodes Read               = InterRead
mapCodes Write              = InterWrite
mapCodes (Loop [Decrement]) = InterSet 0
mapCodes (Loop vals)        = InterLoop $ map mapCodes $ filterComments vals

canBeMerged :: InterpreterCode -> InterpreterCode -> Bool
canBeMerged (InterMov _) (InterMov _)   = True
canBeMerged (InterAdd _) (InterAdd _)   = True
canBeMerged (InterSet _) (InterSet _)   = True
canBeMerged (InterSet _) (InterAdd _)   = True
canBeMerged (InterSet 0) (InterLoop _)  = True
canBeMerged (InterAdd _) (InterSet _)   = True
canBeMerged (InterLoop _) (InterLoop _) = True
canBeMerged _ _                         = False

mergeOperations :: InterpreterCode -> InterpreterCode -> InterpreterCode
mergeOperations (InterMov a) (InterMov b)   = InterMov (a + b)
mergeOperations (InterAdd a) (InterAdd b)   = InterAdd (a + b)
mergeOperations (InterSet a) (InterSet b)   = InterSet b
mergeOperations (InterSet a) (InterAdd b)   = InterSet (a + b)
mergeOperations (InterSet 0) (InterLoop b)  = InterSet 0
mergeOperations (InterAdd a) (InterSet b)   = InterSet b
mergeOperations (InterLoop a) (InterLoop b) = InterLoop a

isNoAction :: InterpreterCode -> Bool
isNoAction action
  | action == InterAdd 0 = True
  | action == InterLoop [] = True
  | otherwise = False

isLoop :: InterpreterCode -> Bool
isLoop (InterLoop _) = True
isLoop _             = False

optimizer ::
     InterpreterCodeBlock -> InterpreterCodeBlock -> InterpreterCodeBlock
optimizer processed [] = processed
optimizer [] other = optimizer [head other] (tail other)
optimizer processed (x:xs)
  | canBeMerged (last processed) x =
    optimizer (init processed ++ [mergeOperations (last processed) x]) xs
  | isLoop x = optimizer (processed ++ [InterLoop (optimizer [] (_block x))]) xs
  | otherwise = optimizer (processed ++ [x]) xs

removeStartLoops :: InterpreterCodeBlock -> InterpreterCodeBlock
removeStartLoops [] = []
removeStartLoops (x:xs) =
  case x of
    InterLoop code -> removeStartLoops xs
    _              -> x : xs

preprocess :: BrainBreakBlock -> InterpreterCodeBlock
preprocess = map mapCodes

optimize :: BrainBreakBlock -> InterpreterCodeBlock
optimize =
  filter (not . isNoAction) .
  optimizer [] . removeStartLoops . preprocess . filterComments
