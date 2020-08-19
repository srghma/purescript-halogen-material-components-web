module HalogenMWC.Chip.Filter.Internal (Chip(..), Config(..))

import Html


type Config r i
    = Config
        { icon :: Maybe String
        , selected :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String
