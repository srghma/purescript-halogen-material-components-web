module HalogenMWC.Select.Item.Internal (Config(..), SelectItem(..))

import Html (Html)


data Config a msg
    = Config
        { value :: a
        , disabled :: Bool
        , additionalAttributes :: Array (IProp r i)
        }


data SelectItem a msg
    = SelectItem (Config a msg) (Array (Html msg))
