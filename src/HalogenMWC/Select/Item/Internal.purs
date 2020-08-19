module HalogenMWC.Select.Item.Internal (Config(..), SelectItem(..))

import Html (Html)


data Config a msg
    = Config
        { value :: a
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


data SelectItem a msg
    = SelectItem (Config a msg) (Array (Html msg))
