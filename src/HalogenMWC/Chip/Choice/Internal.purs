module HalogenMWC.Chip.Choice.Internal (Chip(..), Config(..))

import Html


data Config msg
    = Config
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        }


data Chip a msg
    = Chip (Config msg) a
