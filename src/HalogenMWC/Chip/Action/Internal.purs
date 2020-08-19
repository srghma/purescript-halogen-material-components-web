module HalogenMWC.Chip.Action.Internal (Chip(..), Config(..))

import Html


type Config r i
    = Config
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String
