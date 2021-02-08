module TestUtils where

import           Text.Trifecta

genParser :: Parser a -> String -> a
genParser parse_func code =
  case parseString parse_func mempty code of
    Success parsed -> parsed
    Failure _      -> undefined
