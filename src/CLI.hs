module CLI where

data CompileBackends =
  Cpp
  deriving (Bounded, Enum, Show, Eq)

data MainOptions =
  MainOptions
    { input   :: String
    , output  :: String
    , backend :: CompileBackends
    }
