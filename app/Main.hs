module Main where

import           CLI
import           Interpreter.REPL
import           Lib
import           MainOptions
import           Options

main :: IO ()
main =
  runCommand $ \opts args ->
    case (input opts, output opts) of
      ("", "")          -> startREPL
      (inFile, "")      -> runFile inFile
      (inFile, outFile) -> compileFile inFile outFile $ backend opts
