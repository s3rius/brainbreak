module Lib where

import           REPL
import           Data.List
import           LangParser
import           Definitions
import           Text.Trifecta
import           Control.Monad.State


runFile :: String -> IO ()
runFile filename = do
    parse_result <- parseFromFileEx parseBrainBreak filename
    case parse_result of
        Success code -> evalStateT (runBrainBreakCode code) defaultState
        Failure info -> print (_errDoc info)

compileFile :: String -> String -> IO ()
compileFile input output = putStrLn "Compiling code developing is in progress."
