module Interpreter.Interpreter where

import           System.IO
import           Text.Trifecta
import           Parser.LangParser
import           Parser.Definitions
import           Control.Monad.State
import           Interpreter.Optimizer
import           Interpreter.Definitions


data InterpreterState = InterpreterState{
    buffer :: [Integer],
    inputNumber :: Integer,
    index :: Integer,
    offset :: Integer
} deriving (Show)

-- Initial buffer length and expanding rate
replicaFactor :: Int
replicaFactor = 350

io :: IO a -> StateT InterpreterState IO a
io = liftIO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

defaultState :: InterpreterState
defaultState = InterpreterState { buffer      = replicate replicaFactor 0
                                , inputNumber = 0
                                , index       = 0
                                , offset      = 0
                                }

updateListElement :: [Integer] -> Integer -> (Integer -> Integer) -> [Integer]
updateListElement list index update = do
    let element     = update $ list !! fromInteger index
    let (x, _ : ys) = splitAt (fromInteger index) list
    x ++ element : ys

expandIfNeeded :: [Integer] -> Integer -> [Integer]
expandIfNeeded old_buffer new_index
    | new_index < 0 = replicate replicaFactor 0 ++ old_buffer
    | new_index >= 0 =  old_buffer
    ++ needed (new_index - toInteger (length old_buffer))
  where
    needed len | len >= 0 = replicate replicaFactor 0
               | len < 0  = []

increaseInputNumber :: InterpreterState -> InterpreterState
increaseInputNumber state = state { inputNumber = inputNumber state + 1 }

-- Fuction to update state accordingly to pointer movement 
walkUpdate :: Integer -> InterpreterState -> InterpreterState
walkUpdate steps old_state = do
    let update_func t = t + steps
    let updated_buffer =
            expandIfNeeded (buffer old_state) (update_func $ index old_state)
    -- Finding diff because we need to update current index correctly
    let len_diff =
            toInteger $ length updated_buffer - length (buffer old_state)
    -- Finding new index of our pointer
    let new_index = case () of
            _ | steps < 0 -> update_func $ index old_state + len_diff
              | steps > 0 -> update_func $ index old_state

    old_state { buffer = updated_buffer
              , index  = new_index
              , offset = update_func $ offset old_state
              }

updateStateCell :: (Integer -> Integer) -> InterpreterState -> InterpreterState
updateStateCell update_func old_state = do
    let new_buffer =
            updateListElement (buffer old_state) (index old_state) update_func
    old_state { buffer = new_buffer }

updateCell :: (Integer -> Integer) -> StateT InterpreterState IO ()
updateCell update_func = modify (updateStateCell update_func)

walk :: Integer -> StateT InterpreterState IO ()
walk steps = do
    modify (walkUpdate steps)
    return ()

doAction :: REPLCode -> State InterpreterState ()
doAction = undefined


getBufferSlice :: Integer -> Integer -> [Integer] -> [Integer]
getBufferSlice size current_index current_buffer = do
    let start = fromInteger $ current_index - size
    let end   = fromInteger $ current_index + size
    drop start $ take end current_buffer

getCell :: StateT InterpreterState IO Integer
getCell = do
    state <- get
    return $ buffer state !! fromInteger (index state)

printCell :: StateT InterpreterState IO ()
printCell = do
    cell <- getCell
    io $ putChar (toEnum (fromInteger cell) :: Char)
    io $ hFlush stdout
    return ()

readCell :: StateT InterpreterState IO ()
readCell = do
    char <- io getChar
    updateCell (const $ toInteger $ fromEnum char)
    return ()

runInterpreterLoop :: InterpreterCodeBlock -> StateT InterpreterState IO ()
runInterpreterLoop code = do
    cell <- getCell
    case cell of
        0   -> return ()
        val -> runInterpreterCode code
    cell <- getCell
    case cell of
        0   -> return ()
        val -> runInterpreterLoop code

runInterpreterOperation :: InterpreterCode -> StateT InterpreterState IO ()
runInterpreterOperation (InterAdd value) = updateCell (+ value)
runInterpreterOperation (InterMov step ) = walk step
runInterpreterOperation (InterSet value) = updateCell (const value)
runInterpreterOperation InterRead        = readCell
runInterpreterOperation InterWrite       = printCell
runInterpreterOperation (InterLoop code) = runInterpreterLoop code

runInterpreterCode :: InterpreterCodeBlock -> StateT InterpreterState IO ()
runInterpreterCode (x : xs) = do
    runInterpreterOperation x
    runInterpreterCode xs
runInterpreterCode _ = return ()

runBrainBreakCode :: BrainBreakBlock -> StateT InterpreterState IO ()
runBrainBreakCode = runInterpreterCode . optimize
