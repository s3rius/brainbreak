module Lib where

import           Data.List
import           Text.Trifecta
import           Interpreter.REPL
import           Parser.LangParser
import           Parser.Definitions
import           Control.Monad.State
import           Interpreter.Interpreter


runFile :: String -> IO ()
runFile filename = do
    parse_result <- parseFromFileEx parseBrainBreak filename
    case parse_result of
        Success code -> evalStateT (runBrainBreakCode code) defaultState
        Failure info -> print (_errDoc info)

compileFile :: String -> String -> IO ()
compileFile input output = putStrLn "Compiling code developing is in progress."
