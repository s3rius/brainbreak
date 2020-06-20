module Interpreter.Definitions where


data InterpreterCode = 
    InterAdd Int
    | InterMov Integer
    | InterSet Int
    | InterRead
    | InterWrite
    | InterLoop [InterpreterCode]
    deriving (Eq, Show)

type InterpreterCodeBlock = [InterpreterCode]