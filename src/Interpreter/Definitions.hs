module Interpreter.Definitions where


data InterpreterCode = 
    InterAdd Integer
    | InterMov Integer
    | InterSet Integer
    | InterRead
    | InterWrite
    | InterLoop [InterpreterCode]
    deriving (Eq, Show)

type InterpreterCodeBlock = [InterpreterCode]