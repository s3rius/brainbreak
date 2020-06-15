module Parser.LangParser where
    
import           Data.Maybe                     ( catMaybes )
import           Data.Functor
import           Text.Trifecta
import           Text.Parser.Token
import           Parser.Definitions
import           Control.Applicative
import           Text.Parser.Combinators

-- Remove all comments from code. With recursive Loop code cleaning
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

parseReplHelpers :: Parser REPLHelpers
parseReplHelpers = parsePrintState <|> parsePrintBufChars <|> parsePrintBuf
  where
    parsePrintBuf      = symbol ":buf"   $> PrintBuf
    parsePrintState    = symbol ":state" $> PrintState
    parsePrintBufChars = symbol ":bufc"  $> PrintBufChars

parseREPLCode :: Parser REPLCode
parseREPLCode = Helper <$> parseReplHelpers <|> Code <$> parseBrainBreak

