module Material.Select.Item.Internal exposing (Config(..), SelectItem(..))

import Html exposing (Html)


data Config a msg
    = Config
        { value :: a
        , disabled :: Bool
        , additionalAttributes :: List (Html.Attribute msg)
        }


data SelectItem a msg
    = SelectItem (Config a msg) (List (Html msg))
