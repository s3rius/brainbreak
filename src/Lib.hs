module Lib where

import           LangParser
import           Definitions
import           Text.Trifecta
import           Data.List

startRepl :: IO ()
startRepl = do
    input <- getLine
    let parsed_string = parseString parseBrainBreak mempty input
    case parsed_string of
        Success code -> print (filterComments code)
        Failure info -> print (_errDoc info)
    startRepl

runFile :: String -> IO ()
runFile filename = do
    parse_result <- parseFromFileEx parseBrainBreak filename
    case parse_result of
        Success code -> print (filterComments code)
        Failure info -> print (_errDoc info)


compileFile :: String -> String -> IO ()
compileFile input output = putStrLn "Compiling code in unavailable."

run :: [String] -> IO ()
run args = case args of
    [] -> startRepl
    [x] -> runFile x
    (inFile : _ : "-o" : outFile : _) -> compileFile inFile outFile
