module Material.Chip.Input.Internal exposing (Chip(..), Config(..))

import Html


data Config msg
    = Config
        { leadingIcon :: Maybe String
        , trailingIcon :: Maybe String
        , additionalAttributes :: List (Html.Attribute msg)
        , onClick :: Maybe msg
        , onDelete :: Maybe msg
        }


data Chip msg
    = Chip (Config msg) String
