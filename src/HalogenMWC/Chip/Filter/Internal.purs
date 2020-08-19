module HalogenMWC.Chip.Filter.Internal (Chip(..), Config(..))

import Html


data Config msg
    = Config
        { icon :: Maybe String
        , selected :: Bool
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String
