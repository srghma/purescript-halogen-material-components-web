module HalogenMWC.Select.Item.Internal (Config(..), SelectItem(..))

import Html (Html)


data Config a r i
    = Config
        { value :: a
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


data SelectItem a r i
    = SelectItem (Config a r i) (Array (Html r i))
