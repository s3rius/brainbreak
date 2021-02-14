module Lib where

import           CLI
import           Compiler.Compiler
import           Control.Monad.State
import           Interpreter.Interpreter
import           Interpreter.Optimizer
import           Parser.Parser
import           Text.Trifecta

runFile :: String -> IO ()
runFile filename = do
  parse_result <- parseFromFileEx parseBrainBreak filename
  case parse_result of
    Success code -> evalStateT (runBrainBreakCode code) defaultState
    Failure info -> print (_errDoc info)

compileFile :: String -> String -> CompileBackends -> IO ()
compileFile inputFile outputFile backend = do
  parse_result <- parseFromFileEx parseBrainBreak inputFile
  case parse_result of
    Success code -> compile outputFile backend $ optimize code
    Failure info -> print (_errDoc info)
