module Material.Checkbox.Internal exposing (Config(..), State(..))

import Html


data Config msg
    = Config
        { state :: Maybe State
        , disabled :: Bool
        , additionalAttributes :: List (Html.Attribute msg)
        , onChange :: Maybe msg
        , touch :: Bool
        }


data State
    = Unchecked
    | Checked
    | Indeterminate
