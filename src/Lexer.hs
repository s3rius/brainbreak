module Lexer where
import Control.Applicative
import Text.Trifecta
import Definitions
import Text.Parser.Combinators
import Text.Parser.Token
import Data.Functor

parseBrainBreak :: Parser BrainBreakBlock
parseBrainBreak = many $ parseLeft
 <|> parseRight
 <|> parseIncrement
 <|> parseDecrement
 <|> parseRead
 <|> parseWrite
 <|> parseLoop
    where
        parseRead      = comma        $> Read
        parseWrite     = dot          $> Write
        parseLeft      = symbolic '<' $> MoveLeft
        parseRight     = symbolic '>' $> MoveRight
        parseIncrement = symbolic '+' $> Increment
        parseDecrement = symbolic '-' $> Decrement
        parseLoop      = do
            expr <- brackets parseBrainBreak
            pure $ Loop expr
