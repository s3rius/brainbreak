module Interpreter.Interpreter where

import qualified Data.Map                      as M
import           Data.Map.Strict
import           System.IO
import           Text.Trifecta
import           Parser.LangParser
import           Parser.Definitions
import           Control.Monad.State
import           Interpreter.Optimizer
import           Interpreter.Definitions


data InterpreterState = InterpreterState{
    buffer :: M.Map Integer Int,
    inputNumber :: Integer,
    index :: Integer,
    offset :: Integer
} deriving (Show)


io :: IO a -> StateT InterpreterState IO a
io = liftIO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

defaultState :: InterpreterState
defaultState = InterpreterState { buffer      = M.empty
                                , inputNumber = 0
                                , index       = 0
                                , offset      = 0
                                }

getBufferSlice :: Integer -> Integer ->  M.Map Integer Int -> [Int]
getBufferSlice size current_index current_buffer = do
    let start        = fromInteger $ current_index - size
    let end          = fromInteger $ current_index + size
    let map_array = [start .. end]
    Prelude.map (\el -> findWithDefault 0 el current_buffer) map_array

increaseInputNumber :: InterpreterState -> InterpreterState
increaseInputNumber state = state { inputNumber = inputNumber state + 1 }

-- Fuction to update state accordingly to pointer movement 
walkUpdate :: Integer -> InterpreterState -> InterpreterState
walkUpdate steps old_state = do
    let update_func t = t + steps
    -- Finding new index of our pointer
    old_state { index  = update_func $ index old_state
              , offset = update_func $ offset old_state
              }

updateStateCell :: (Int -> Int) -> InterpreterState -> InterpreterState
updateStateCell update_func old_state = do
    let value = findWithDefault 0 (index old_state) (buffer old_state)
    let new_buffer = insert (index old_state) (update_func value)  (buffer old_state)
    old_state { buffer = new_buffer }

updateCell :: (Int -> Int) -> StateT InterpreterState IO ()
updateCell update_func = modify (updateStateCell update_func)

walk :: Integer -> StateT InterpreterState IO ()
walk steps = do
    modify (walkUpdate steps)
    return ()

doAction :: REPLCode -> State InterpreterState ()
doAction = undefined

getCell :: StateT InterpreterState IO Int
getCell = do
    state <- get
    return $ findWithDefault 0 (index state) (buffer state)

printCell :: StateT InterpreterState IO ()
printCell = do
    cell <- getCell
    io $ putChar (toEnum cell :: Char)
    io $ hFlush stdout
    return ()

readCell :: StateT InterpreterState IO ()
readCell = do
    char <- io getChar
    updateCell (const $ fromEnum char)
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