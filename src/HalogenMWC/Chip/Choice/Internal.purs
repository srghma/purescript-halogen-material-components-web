module HalogenMWC.Chip.Choice.Internal (Chip(..), Config(..))

import Html


type Config r i
    = Config
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        }


data Chip a msg
    = Chip (Config msg) a
