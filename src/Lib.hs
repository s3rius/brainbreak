module Lib where

import           Compiler.Compiler
import           Control.Monad.State
import           Interpreter.Interpreter
import           Parser.Parser
import           Text.Trifecta

runFile :: String -> IO ()
runFile filename = do
  parse_result <- parseFromFileEx parseBrainBreak filename
  case parse_result of
    Success code -> evalStateT (runBrainBreakCode code) defaultState
    Failure info -> print (_errDoc info)

compileFile :: String -> String -> IO ()
compileFile inputFile outputFile = do
  parse_result <- parseFromFileEx parseBrainBreak inputFile
  case parse_result of
    Success code -> putStrLn $ compileBrainBreak code
    Failure info -> print (_errDoc info)
