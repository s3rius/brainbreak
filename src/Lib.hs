module Lib where

import Lexer
import Text.Trifecta

parseLine :: String -> IO ()
parseLine line = case parseString parseBrainBreak mempty line of
                    Success code -> print $ show code
                    Failure info -> print $ _errDoc info
