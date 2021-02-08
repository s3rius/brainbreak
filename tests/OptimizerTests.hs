module OptimizerTests
  ( testOptimization
  , testPreprocessing
  ) where

import           Interpreter.Definitions
import           Interpreter.Optimizer
import           Parser.Definitions
import           Parser.LangParser
import           Test.Hspec
import           TestUtils

testOptimize :: String -> InterpreterCodeBlock
testOptimize = optimize . genParser parseBrainBreak

testPreprocess :: String -> InterpreterCodeBlock
testPreprocess = preprocess . filterComments . genParser parseBrainBreak

testOptimization :: IO ()
testOptimization =
  hspec $ do
    describe "BrainBreak optimizer" $ do
      it "can merge instructions" $ do
        testOptimize "+++" `shouldBe` [InterAdd 3]
        testOptimize ">>>" `shouldBe` [InterMov 3]
        testOptimize "[-]+++" `shouldBe` [InterSet 3]
        testOptimize "+++[-]" `shouldBe` [InterSet 0]
      it "removes loops at start" $ do
        testOptimize "[+]+++" `shouldBe` [InterAdd 3]
        testOptimize "[+][+]+++" `shouldBe` [InterAdd 3]

testPreprocessing :: IO ()
testPreprocessing =
  hspec $ do
    describe "BrainBreak preprocessor" $ do
      it "can map parser instructions" $ do
        testPreprocess "++" `shouldBe` [InterAdd 1, InterAdd 1]
        testPreprocess "--" `shouldBe` [InterAdd (-1), InterAdd (-1)]
        testPreprocess ">>" `shouldBe` [InterMov 1, InterMov 1]
        testPreprocess "<<" `shouldBe` [InterMov (-1), InterMov (-1)]
        testPreprocess ",," `shouldBe` [InterRead, InterRead]
        testPreprocess ".." `shouldBe` [InterWrite, InterWrite]
        testPreprocess "[+]" `shouldBe` [InterLoop [InterAdd 1]]
        testPreprocess "[-]" `shouldBe` [InterSet 0]