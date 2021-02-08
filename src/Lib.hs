module Lib where

import           Control.Monad.State
import           Data.List
import           Interpreter.Interpreter
import           Interpreter.REPL
import           Parser.Definitions
import           Parser.LangParser
import           Text.Trifecta

runFile :: String -> IO ()
runFile filename = do
  parse_result <- parseFromFileEx parseBrainBreak filename
  case parse_result of
    Success code -> evalStateT (runBrainBreakCode code) defaultState
    Failure info -> print (_errDoc info)

compileFile :: String -> String -> IO ()
compileFile input output = putStrLn "Compiling code developing is in progress."
