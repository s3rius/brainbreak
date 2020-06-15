module Interpreter.REPL where

import           System.IO
import           Text.Trifecta
import           Parser.LangParser
import           Parser.Definitions
import           Control.Monad.State
import           Interpreter.Interpreter



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