module Main where

import Lib

main :: IO ()
main = do 
    x <- getLine
    parseLine x