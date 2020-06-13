module Lib where

import LangParser
import Definitions
import Text.Trifecta

parseLine :: String -> IO ()
parseLine line = case parseString parseBrainBreak mempty line of
                    Success code -> print $ show $ filterComments code
                    Failure info -> print $ _errDoc info
