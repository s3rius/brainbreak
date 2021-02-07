module ParserTests where

import TestUtils ( genParser )
import Test.Hspec
import Parser.LangParser
import Parser.Definitions

testParser :: IO()
testParser = hspec $ do
    let parse = genParser parseBrainBreak
    describe "BrainBreak parser" $ do
        it "can parse moving" $ do
            parse ">" `shouldBe` [MoveRight]
            parse "<" `shouldBe` [MoveLeft]
        it "can parse arithmetic" $ do
            parse "+" `shouldBe` [Increment]
            parse "-" `shouldBe` [Decrement] 
        it "can parse io operation" $ do
            parse "," `shouldBe` [Read]
            parse "." `shouldBe` [Write]
        it "can parse comments" $ do
            parse "a" `shouldBe` [Comment]
        it "can parse loops" $ do
            parse "[-]" `shouldBe` [Loop[Decrement]]
            parse "[+]" `shouldBe` [Loop[Increment]]
            parse "[>]" `shouldBe` [Loop[MoveRight]]
            parse "[<]" `shouldBe` [Loop[MoveLeft]]

testRepl :: IO()
testRepl = hspec $ do
    let parse = genParser parseREPLCode
    describe "BrainBreak repl" $ do
        it "can parse repl commands" $ do
            parse ":buf"   `shouldBe` Helper PrintBuf
            parse ":bufc"  `shouldBe` Helper PrintBufChars
            parse ":state" `shouldBe` Helper PrintState
        it "can parse BrainBreak" $ do
            parse "+>" `shouldBe` Code [Increment, MoveRight]


testCommentFilter :: IO()
testCommentFilter = hspec $ do
    let parse = genParser parseBrainBreak
    describe "Comment Filter" $ do
        it "can filter plain comments" $ do
            (filterComments . parse) "test+" `shouldBe` [Increment]
        it "can filter nested comments" $ do
            (filterComments . parse) "t[test+]" `shouldBe` [Loop[Increment]]