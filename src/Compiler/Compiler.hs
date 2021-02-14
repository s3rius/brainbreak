module Compiler.Compiler where

import           Compiler.Backends.C.Main
import           Interpreter.Definitions

compile :: String -> InterpreterCodeBlock -> IO ()
compile = compileC
