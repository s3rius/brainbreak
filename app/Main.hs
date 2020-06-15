module Main where

import           Lib
import           Options
import           MainOptions
import           Interpreter.REPL
import           System.Environment

main :: IO ()
main = runCommand $ \opts args -> case (input opts, output opts) of
    (""    , ""     ) -> startREPL  
    (inFile, ""     ) -> runFile inFile
    (inFile, outFile) -> compileFile inFile outFile
