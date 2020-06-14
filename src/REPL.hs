module REPL where

import           Control.Monad.State
import           Definitions

data REPLState = REPLState{
    buffer :: [Integer],
    index :: Integer,
    offset :: Integer
} deriving (Show)

data Direction =
    LeftDirection | RightDirection
    deriving (Eq, Show)


-- Initial buffer length and expanding rate
replicaFactor :: Int
replicaFactor = 3000

defaultState :: REPLState
defaultState =
    REPLState { buffer = replicate replicaFactor 0, index = 0, offset = 0 }

expandIfNeeded :: [Integer] -> Integer -> [Integer]
-- Expand buffer to travel left but index is less than zero
expandIfNeeded old_buffer (-1)      = replicate replicaFactor 0 ++ old_buffer
-- Expand buffer in need to travel Right
expandIfNeeded old_buffer new_index = old_buffer
    ++ needed (new_index - toInteger (length old_buffer))
  where
    needed len | len >= 0 = replicate replicaFactor 0
               | len < 0  = []

-- Fuction to update state accordingly to walk direction
walkUpdate :: Direction -> REPLState -> REPLState
-- Walk left with buffer expnding if needed
walkUpdate LeftDirection old_state  = do
    let update_func t = t - 1
    let updated_buffer =
            expandIfNeeded (buffer old_state) (update_func $ index old_state)
    -- Finding diff because we need to update current index correctly
    let len_diff =
            toInteger $ length updated_buffer - length (buffer old_state)
    REPLState { buffer = updated_buffer
              , index  = update_func $ index old_state + len_diff
              , offset = update_func $ offset old_state
              }

walkUpdate RightDirection old_state  = do
    let update_func t = t + 1
    let updated_buffer =
            expandIfNeeded (buffer old_state) (update_func $ index old_state)
    REPLState { buffer = updated_buffer
              , index  = update_func $ index old_state
              , offset = update_func $ offset old_state
              }

walk :: Direction -> State REPLState ()
walk direction = do
    modify (walkUpdate direction)
    return ()

doAction :: REPLCode -> State REPLState ()
doAction = undefined

runHelper :: REPLHelpers -> State REPLState ()
runHelper = undefined

runReplCode :: REPLCode -> IO ()
runReplCode = undefined