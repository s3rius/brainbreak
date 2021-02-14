module Parser.ParserSpec where

import           Parser.Definitions
import           Parser.Parser
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = do
  let parse_bb = genParser parseBrainBreak
  describe "BrainBreak parser" $ do
    it "can parse moving" $ do
      parse_bb ">" `shouldBe` [MoveRight]
      parse_bb "<" `shouldBe` [MoveLeft]
    it "can parse arithmetic" $ do
      parse_bb "+" `shouldBe` [Increment]
      parse_bb "-" `shouldBe` [Decrement]
    it "can parse io operation" $ do
      parse_bb "," `shouldBe` [Read]
      parse_bb "." `shouldBe` [Write]
    it "can parse comments" $ parse_bb "a" `shouldBe` [Comment]
    it "can parse loops" $ do
      parse_bb "[-]" `shouldBe` [Loop [Decrement]]
      parse_bb "[+]" `shouldBe` [Loop [Increment]]
      parse_bb "[>]" `shouldBe` [Loop [MoveRight]]
      parse_bb "[<]" `shouldBe` [Loop [MoveLeft]]
  describe "Comment Filter" $ do
    it "can filter plain comments" $
      (filterComments . parse_bb) "test+" `shouldBe` [Increment]
    it "can filter nested comments" $
      (filterComments . parse_bb) "t[test+]" `shouldBe` [Loop [Increment]]
  let parse_repl = genParser parseREPLCode
  describe "BrainBreak repl" $ do
    it "can parse repl commands" $ do
      parse_repl ":buf" `shouldBe` Helper PrintBuf
      parse_repl ":bufc" `shouldBe` Helper PrintBufChars
      parse_repl ":state" `shouldBe` Helper PrintState
    it "can parse BrainBreak" $
      parse_repl "+>" `shouldBe` Code [Increment, MoveRight]
