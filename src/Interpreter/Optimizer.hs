module Interpreter.Optimizer where 

{- 
Optiomization plans:
 * Instruction merging
 * Efficient cell zeroing
 * Dead code elimination
 * Instruction conversion

 ## Insstruction merging
 Precalculate the result of operation brefore compile/During interpreting.
    +++ --- ++ => ++
    +++ --- --- => ---
    >>> << => >
    > <<<< => <<<<
    +++ --- => Deletes operation

## Efficient cell zeroing
    [-] -> Set current cell to zero

## Dead code elimination
    Because the next cell is probably zero, delete the second loop.
    Maybe this check is unnecesary.
    [+>][<-] => [+>]
    Also remove Loops at the begining of the programm.

## Instruction conversion
    Just for efficency
    [-]+++ => Set 0, Add 3 => Set 3
    +++[-] => Set 0
    Also sequential Set instructions may be merged.
-}