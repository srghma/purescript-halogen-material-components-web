module HalogenMWC.Chip.Action.Internal (Chip(..), Config(..))

import Html


data Config msg
    = Config
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String
