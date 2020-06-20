module Interpreter.REPL where

import           System.IO
import           Text.Trifecta
import           Parser.LangParser
import           Parser.Definitions
import           Control.Monad.State
import           Interpreter.Interpreter

runHelper :: REPLHelpers -> StateT InterpreterState IO ()
runHelper PrintState = do
    current_state <- get
    let buffer_slice =
            getBufferSlice 5 (index current_state) (buffer current_state)
    io $ putStrLn $ "Current index: " ++ show (index current_state)
    io $ putStrLn $ "Offset from start: " ++ show (offset current_state)
    io $ putStrLn $ "part of curren buffer: \n" ++ show buffer_slice
    return ()

runHelper PrintBuf = do
    current_state <- get
    let buffer_slice =
            getBufferSlice 5 (index current_state) (buffer current_state)
    io $ print buffer_slice
    return ()

runHelper PrintBufChars = do
    current_state <- get
    let buffer_slice =
            getBufferSlice 5 (index current_state) (buffer current_state)
    io $ print $ map (\t -> toEnum t :: Char) buffer_slice
    return ()


replCodeRunner :: REPLCode -> StateT InterpreterState IO ()
replCodeRunner code = case code of
    Helper helper  -> runHelper helper
    Code   bb_code -> runBrainBreakCode $ filterComments bb_code

runREPL :: StateT InterpreterState IO ()
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