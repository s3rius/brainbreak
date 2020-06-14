module LangParser where
import           Control.Applicative
import           Text.Trifecta
import           Data.Maybe                     ( catMaybes )
import           Definitions
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Data.Functor

filterComments :: BrainBreakBlock -> BrainBreakBlock
filterComments (Comment : ops) = [] ++ filterComments ops
filterComments (Loop ops : otherCode) =
    Loop (filterComments ops) : filterComments otherCode
filterComments (op : ops) = op : filterComments ops
filterComments []         = []

parseComments :: Parser BrainBreakOperation
parseComments = satisfy (not . (`elem` "<+>-[],.")) $> Comment

parseLoop :: Parser BrainBreakOperation
parseLoop = do
    expr <- brackets parseBrainBreak
    pure $ Loop expr

parseBrainBreak :: Parser BrainBreakBlock
parseBrainBreak =
    many
        $   parseLeft
        <|> parseRight
        <|> parseIncrement
        <|> parseDecrement
        <|> parseRead
        <|> parseWrite
        <|> parseLoop
        <|> parseComments
  where
    parseRead      = comma $> Read
    parseWrite     = dot $> Write
    parseLeft      = symbolic '<' $> MoveLeft
    parseRight     = symbolic '>' $> MoveRight
    parseIncrement = symbolic '+' $> Increment
    parseDecrement = symbolic '-' $> Decrement
