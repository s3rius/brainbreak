module CodeGenerator where

import Definitions

data State = State {
    parsed :: BrainBreakBlock,
    unparsed :: BrainBreakBlock,
    mainFunction :: String
}

