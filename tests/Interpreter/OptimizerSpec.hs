module Interpreter.OptimizerSpec where

import           Interpreter.Definitions
import           Interpreter.Optimizer
import           Parser.Parser
import           Test.Hspec
import           TestUtils

testOptimize :: String -> InterpreterCodeBlock
testOptimize = optimize . genParser parseBrainBreak

testPreprocess :: String -> InterpreterCodeBlock
testPreprocess = preprocess . filterComments . genParser parseBrainBreak

spec :: Spec
spec = do
  describe "BrainBreak optimizer" $ do
    it "can merge instructions" $ do
      testOptimize "+++" `shouldBe` [InterAdd 3]
      testOptimize ">>>" `shouldBe` [InterMov 3]
      testOptimize "[-]+++" `shouldBe` [InterSet 3]
      testOptimize "+++[-]" `shouldBe` [InterSet 0]
      testOptimize ">[-][+>]" `shouldBe` [InterMov 1, InterSet 0]
      testOptimize ">[>>>]" `shouldBe` [InterMov 1, InterLoop [InterMov 3]]
    it "removes usless statements" $ do
      testOptimize ">+-" `shouldBe` [InterMov 1]
      testOptimize "><>" `shouldBe` [InterMov 1]
      testOptimize ">><" `shouldBe` [InterMov 1]
      testOptimize ">[]" `shouldBe` [InterMov 1]
    it "removes loops at start" $ do
      testOptimize "[+]+++" `shouldBe` [InterAdd 3]
      testOptimize "[+][+]" `shouldBe` []
  describe "BrainBreak preprocessor" $
    it "can map parser instructions" $ do
      testPreprocess "++" `shouldBe` [InterAdd 1, InterAdd 1]
      testPreprocess "--" `shouldBe` [InterAdd (-1), InterAdd (-1)]
      testPreprocess ">>" `shouldBe` [InterMov 1, InterMov 1]
      testPreprocess "<<" `shouldBe` [InterMov (-1), InterMov (-1)]
      testPreprocess ",," `shouldBe` [InterRead, InterRead]
      testPreprocess ".." `shouldBe` [InterWrite, InterWrite]
      testPreprocess "[+]" `shouldBe` [InterLoop [InterAdd 1]]
      testPreprocess "[-]" `shouldBe` [InterSet 0]
