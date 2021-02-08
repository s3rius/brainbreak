import ParserTests
import OptimizerTests

main :: IO ()
main = do 
    testRepl
    testParser
    testCommentFilter
    testPreprocessing
    testOptimization
    