module Main where

import           Lib
import           Options
import           MainOptions
import           System.Environment

main :: IO ()
main = runCommand $ \opts args -> case (input opts, output opts) of
    (""    , ""     ) -> startRepl
    (inFile, ""     ) -> runFile inFile
    (inFile, outFile) -> compileFile inFile outFile
