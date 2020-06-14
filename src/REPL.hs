module REPL where

import           LangParser
import           Definitions
import           Text.Trifecta
import           Control.Monad.State
import           System.IO

data REPLState = REPLState{
    buffer :: [Integer],
    inputNumber :: Integer,
    index :: Integer,
    offset :: Integer
} deriving (Show)

data Direction =
    LeftDirection | RightDirection
    deriving (Eq, Show)


-- Initial buffer length and expanding rate
replicaFactor :: Int
replicaFactor = 3000

io :: IO a -> StateT REPLState IO a
io = liftIO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

defaultState :: REPLState
defaultState = REPLState { buffer      = replicate replicaFactor 0
                         , inputNumber = 0
                         , index       = 0
                         , offset      = 0
                         }

updateListElement :: [Integer] -> Integer -> (Integer -> Integer) -> [Integer]
updateListElement list index update = do
    let element = update $ list !! fromInteger index
    let (x,_:ys) = splitAt (fromInteger index) list
    x ++ element : ys

expandIfNeeded :: [Integer] -> Integer -> [Integer]
-- Expand buffer to travel left but index is less than zero
expandIfNeeded old_buffer (-1)      = replicate replicaFactor 0 ++ old_buffer
-- Expand buffer in need to travel Right
expandIfNeeded old_buffer new_index = old_buffer
    ++ needed (new_index - toInteger (length old_buffer))
  where
    needed len | len >= 0 = replicate replicaFactor 0
               | len < 0  = []

increaseInputNumber :: REPLState -> REPLState
increaseInputNumber state = state { inputNumber = inputNumber state + 1 }

-- Fuction to update state accordingly to walk direction
walkUpdate :: Direction -> REPLState -> REPLState
-- Walk left with buffer expnding if needed
walkUpdate LeftDirection old_state = do
    let update_func t = t - 1
    let updated_buffer =
            expandIfNeeded (buffer old_state) (update_func $ index old_state)
    -- Finding diff because we need to update current index correctly
    let len_diff =
            toInteger $ length updated_buffer - length (buffer old_state)
    old_state { buffer = updated_buffer
              , index  = update_func $ index old_state + len_diff
              , offset = update_func $ offset old_state
              }

walkUpdate RightDirection old_state = do
    let update_func t = t + 1
    let updated_buffer =
            expandIfNeeded (buffer old_state) (update_func $ index old_state)
    old_state { buffer = updated_buffer
              , index  = update_func $ index old_state
              , offset = update_func $ offset old_state
              }

updateStateCell :: (Integer -> Integer) -> REPLState -> REPLState
updateStateCell update_func old_state = do
    let new_buffer = updateListElement (buffer old_state) (index old_state) update_func
    old_state{
        buffer = new_buffer
    }

updateCell :: (Integer->Integer) -> StateT REPLState IO ()
updateCell update_func = modify (updateStateCell update_func)

walk :: Direction -> StateT REPLState IO ()
walk direction = do
    modify (walkUpdate direction)
    return ()

doAction :: REPLCode -> State REPLState ()
doAction = undefined


getBufferSlice :: Integer -> Integer ->  [Integer] -> [Integer]
getBufferSlice size current_index current_buffer = do
    let start        = fromInteger $ current_index - size
    let end          = fromInteger $ current_index + size
    drop start $ take end current_buffer

runHelper :: REPLHelpers -> StateT REPLState IO ()
runHelper PrintState = do
    current_state <- get
    let buffer_slice = getBufferSlice 5 (index current_state) (buffer current_state)
    io $ putStrLn $ "Current index: " ++ show (index current_state)
    io $ putStrLn $ "Offset from start: " ++ show (offset current_state)
    io $ putStrLn $ "part of curren buffer: \n" ++ show buffer_slice
    return ()

runHelper PrintBuf = do
    current_state <- get
    let buffer_slice = getBufferSlice 5 (index current_state) (buffer current_state)
    io $ print buffer_slice
    return ()

runHelper PrintBufChars  = do
    current_state <- get
    let buffer_slice = getBufferSlice 5 (index current_state) (buffer current_state)
    io $ print $ map (\t -> toEnum (fromInteger t) :: Char) buffer_slice
    return ()

getCell :: StateT REPLState IO Integer
getCell = do
    state <- get
    return $ buffer state !! fromInteger (index state)

printCell :: StateT REPLState IO ()
printCell = do
    cell <- getCell
    io $ putChar (toEnum (fromInteger cell) :: Char)
    io $ hFlush stdout
    return ()

readCell :: StateT REPLState IO ()
readCell = do
    char <- io getChar
    updateCell (const $ toInteger $ fromEnum char)
    return ()

runBrainCodeLoop :: BrainBreakBlock -> StateT REPLState IO ()
runBrainCodeLoop  code = do
    cell <- getCell
    case cell of
        0 -> return ()
        val -> runBrainBreakCode code
    cell <- getCell
    case cell of 
        0 -> return ()
        val -> runBrainCodeLoop code

runBrainBreakOperation :: BrainBreakOperation -> StateT REPLState IO ()
runBrainBreakOperation MoveLeft         = walk LeftDirection
runBrainBreakOperation MoveRight        = walk RightDirection
runBrainBreakOperation Increment        = updateCell (+1)
runBrainBreakOperation Decrement        = updateCell (\t -> t - 1)
runBrainBreakOperation Write            = printCell
runBrainBreakOperation Read             = readCell
runBrainBreakOperation (Loop code)      = runBrainCodeLoop code
runBrainBreakOperation Comment          = return ()

runBrainBreakCode :: BrainBreakBlock -> StateT REPLState IO ()
runBrainBreakCode (x : xs) = do
    runBrainBreakOperation x
    runBrainBreakCode xs
runBrainBreakCode [] = return ()


replCodeRunner :: REPLCode -> StateT REPLState IO ()
replCodeRunner code = case code of
    Helper helper  -> runHelper helper
    Code   bb_code -> runBrainBreakCode $ filterComments bb_code

runREPL :: StateT REPLState IO ()
runREPL = do
    state <- get
    io $ putStr $ "\nIn [" ++ show (inputNumber state) ++ "]: "
    io $ hFlush stdout
    input <- io getLine
    let code = parseString parseREPLCode mempty input
    case code of
        Success code -> replCodeRunner code
        Failure info -> io $ print (_errDoc info)
    modify increaseInputNumber
    runREPL


startREPL :: IO ()
startREPL = evalStateT runREPL defaultState