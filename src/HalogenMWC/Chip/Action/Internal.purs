module Material.Chip.Action.Internal exposing (Chip(..), Config(..))

import Html


data Config msg
    = Config
        { icon :: Maybe String
        , additionalAttributes :: List (Html.Attribute msg)
        , onClick :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String
