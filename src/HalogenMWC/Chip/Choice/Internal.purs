module HalogenMWC.Chip.Choice.Internal (Chip(..), Config(..))

import Html


data Config msg
    = Config
        { icon :: Maybe String
        , additionalAttributes :: List (Html.Attribute msg)
        }


data Chip a msg
    = Chip (Config msg) a
