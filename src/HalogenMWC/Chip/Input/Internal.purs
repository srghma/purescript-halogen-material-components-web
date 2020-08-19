module HalogenMWC.Chip.Input.Internal (Chip(..), Config(..))

import Html


type Config r i
    = Config
        { leadingIcon :: Maybe String
        , trailingIcon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        , onDelete :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String
