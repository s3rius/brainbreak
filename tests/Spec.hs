import           OptimizerTests
import           ParserTests

main :: IO ()
main = do
  testRepl
  testParser
  testCommentFilter
  testPreprocessing
  testOptimization
