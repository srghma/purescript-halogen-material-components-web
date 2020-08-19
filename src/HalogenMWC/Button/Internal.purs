module HalogenMWC.Button.Internal (Config(..))

import Html


type Config r i
    = Config
        { icon :: Maybe String
        , trailingIcon :: Bool
        , disabled :: Bool
        , dense :: Bool
        , href :: Maybe String
        , target :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        , touch :: Bool
        }
