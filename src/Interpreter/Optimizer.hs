module Interpreter.Optimizer
    ( optimize
    )
where

import           Parser.Definitions
import           Parser.LangParser
import           Interpreter.Definitions
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
mapCodes (Loop vals       ) = InterLoop $ map mapCodes $ filterComments vals


canBeMerged :: InterpreterCode -> InterpreterCode -> Bool
canBeMerged (InterMov  _) (InterMov  _) = True
canBeMerged (InterAdd  _) (InterAdd  _) = True
canBeMerged (InterSet  _) (InterSet  _) = True
canBeMerged (InterSet  _) (InterAdd  _) = True
canBeMerged (InterAdd  _) (InterSet  _) = True
canBeMerged (InterLoop _) (InterLoop _) = True
canBeMerged _             _             = False


mergeOperations :: InterpreterCode -> InterpreterCode -> InterpreterCode
mergeOperations (InterMov  a) (InterMov  b) = InterMov (a + b)
mergeOperations (InterAdd  a) (InterAdd  b) = InterAdd (a + b)
mergeOperations (InterSet  a) (InterSet  b) = InterSet b
mergeOperations (InterSet  a) (InterAdd  b) = InterSet (a + b)
mergeOperations (InterAdd  a) (InterSet  b) = InterSet b
mergeOperations (InterLoop a) (InterLoop b) = InterLoop a
mergeOperations _             _             = error "Operations can't be merged"

isNoAction :: InterpreterCode -> Bool
isNoAction action | action == InterMov 0   = True
                  | action == InterAdd 0   = True
                  | action == InterLoop [] = True
                  | otherwise              = False

isOpositeMovement :: InterpreterCode -> InterpreterCode -> Bool
isOpositeMovement (InterMov 1) (InterMov (-1)) = True
isOpositeMovement _            _               = False

optimizer
    :: InterpreterCodeBlock -> InterpreterCodeBlock -> InterpreterCodeBlock
optimizer processed []    = processed
optimizer []        other = optimizer [head other] (tail other)
optimizer processed (x : xs)
    | canBeMerged (last processed) x = optimizer
        (init processed ++ [mergeOperations (last processed) x])
        xs
    | otherwise = optimizer (processed ++ [x]) xs


removeStartLoops :: InterpreterCodeBlock -> InterpreterCodeBlock
removeStartLoops [] = []
removeStartLoops (x : xs) | canBeMerged x (InterLoop []) = removeStartLoops xs
                          | otherwise                    = xs

preprocess :: BrainBreakBlock -> InterpreterCodeBlock
preprocess = map mapCodes

optimize :: BrainBreakBlock -> InterpreterCodeBlock
optimize =
    filter (not . isNoAction)
        . optimizer []
        . removeStartLoops
        . preprocess
        . filterComments
