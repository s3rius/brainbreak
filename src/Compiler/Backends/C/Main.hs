{-# LANGUAGE QuasiQuotes #-}

module Compiler.Backends.C.Main where

import           Compiler.Backends.C.Builder
import           Compiler.Backends.C.Tanslator
import           Data.String.Interpolate
import           Interpreter.Definitions
import           System.Directory
import           System.IO
import           System.Process

compileC :: String -> InterpreterCodeBlock -> IO ()
compileC output_file code = do
  let cpp_file = output_file ++ ".cpp"
  source_file <- openFile cpp_file WriteMode
  putStrLn "Generating C++ code"
  let c_source = (translateC . buildModule) code
  putStrLn "Writing code in file"
  hPutStr source_file c_source
  hClose source_file
  putStrLn "Compiling with 'clang++ -O3'"
  callCommand [i|clang++ -Wall -O3 -o #{output_file} #{cpp_file}|]
  removeFile cpp_file
