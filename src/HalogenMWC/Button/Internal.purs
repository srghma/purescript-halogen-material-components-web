module HalogenMWC.Button.Internal (Config(..))

import Html


data Config msg
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
