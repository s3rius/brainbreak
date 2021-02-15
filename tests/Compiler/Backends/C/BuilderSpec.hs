module Compiler.Backends.C.BuilderSpec where

import           Compiler.Backends.C.AST
import           Compiler.Backends.C.Builder
import           Control.Monad.State
import           Interpreter.OptimizerSpec
import           Test.Hspec

testBuild :: BuilderState -> String -> [COperation]
testBuild initialState code =
  evalState (parseInterpreterCode (testOptimize code)) initialState

testModule :: String -> CModule
testModule = buildModule . testOptimize

spec :: Spec
spec = do
  let counter = CVar "counter" CTypeInt
  let buffer = CVar "buffer" (CTypeMap CTypeInt CTypeInt)
  let initS = BuilderState {_buffer = buffer, _counter = counter}
  let element = MapElement buffer counter
  describe "C++ Module Builder" $ do
    it "can map instructions correctly" $ do
      testBuild initS "+++" `shouldBe`
        [CAdd element element (CValueConst (CConstInt 3))]
      testBuild initS "---" `shouldBe`
        [CAdd element element (CValueConst (CConstInt (-3)))]
      testBuild initS ">>>" `shouldBe`
        [CAdd counter counter (CValueConst (CConstInt 3))]
      testBuild initS "<<<" `shouldBe`
        [CAdd counter counter (CValueConst (CConstInt (-3)))]
      testBuild initS "[-]" `shouldBe`
        [CSet element (CValueConst (CConstInt 0))]
      testBuild initS "+[>-]" `shouldBe`
        [ CAdd element element (CValueConst (CConstInt 1))
        , CLoop
            element
            [ CAdd counter counter (CValueConst (CConstInt 1))
            , CAdd element element (CValueConst (CConstInt (-1)))
            ]
        ]
      testBuild initS ".," `shouldBe` [CPrint element, CRead element]
    it "can build module" $ do
      let (CModule headers ops) = testModule ">"
      headers `shouldBe`
        [CInclude "cstdio", CInclude "unordered_map", CUsingNamespace "std"]
      last ops `shouldBe` CAdd counter counter (CValueConst (CConstInt 1))
