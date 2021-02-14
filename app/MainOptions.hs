module MainOptions where

import           CLI
import           Control.Applicative
import           Options

instance Options MainOptions where
  defineOptions =
    MainOptions <$>
    defineOption
      optionType_string
      (\o ->
         o
           { optionLongFlags = ["input"]
           , optionShortFlags = ['i']
           , optionDescription = "Input file with brainBreak code"
           }) <*>
    defineOption
      optionType_string
      (\o ->
         o
           { optionLongFlags = ["output"]
           , optionShortFlags = ['o']
           , optionDescription = "Compiled file output"
           }) <*>
    defineOption
      (optionType_enum "CompileBackends")
      (\o ->
         o
           { optionLongFlags = ["backend"]
           , optionShortFlags = ['b']
           , optionDescription = "Change compilation backend"
           , optionDefault = Cpp
           })
