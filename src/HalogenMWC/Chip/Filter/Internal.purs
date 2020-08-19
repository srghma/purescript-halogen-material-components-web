module Material.Chip.Filter.Internal exposing (Chip(..), Config(..))

import Html


data Config msg
    = Config
        { icon :: Maybe String
        , selected :: Bool
        , additionalAttributes :: List (Html.Attribute msg)
        , onChange :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String