module HalogenMWC.Select.Item.Internal (Config(..), SelectItem(..))

import Html (Html)


data Config a msg
    = Config
        { value :: a
        , disabled :: Bool
        , additionalAttributes :: List (Html.Attribute msg)
        }


data SelectItem a msg
    = SelectItem (Config a msg) (List (Html msg))
