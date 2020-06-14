module MainOptions where

import           Options
import           Control.Applicative


data MainOptions = MainOptions
    {
        input :: String,
        output :: String
    }


instance Options MainOptions where
    defineOptions =
        MainOptions
            <$> defineOption
                    optionType_string
                    (\o -> o
                        { optionLongFlags   = ["input"]
                        , optionShortFlags  = ['i']
                        , optionDescription = "Input file with brainBreak code"
                        }
                    )
            <*> defineOption
                    optionType_string
                    (\o -> o { optionLongFlags   = ["output"]
                             , optionShortFlags  = ['o']
                             , optionDescription = "Compiled file output"
                             }
                    )