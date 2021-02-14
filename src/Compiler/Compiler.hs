module Compiler.Compiler where

import           CLI
import           Compiler.Backends.C.Main
import           Interpreter.Definitions

compile :: String -> CompileBackends -> InterpreterCodeBlock -> IO ()
compile out_file backend code =
  case backend of
    Cpp -> compileC out_file code
