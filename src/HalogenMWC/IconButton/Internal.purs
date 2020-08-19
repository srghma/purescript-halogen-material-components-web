module HalogenMWC.IconButton.Internal (Config(..))

import Html


type Config r i
    = Config
        { disabled :: Bool
        , label :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        }
