module HalogenMWC.Select.Item
    ( Config, config

    , SelectItem, selectItem
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Select.Item (Config(..), SelectItem(..))

data Config a r i =
    Material.Select.Item.Config a r i

config :: { value :: a } -> Config a r i
config { value } =
        { value: value
        , disabled: False
        , additionalAttributes: []
        }

data SelectItem a r i =
    Material.Select.Item.SelectItem a r i

selectItem :: Config a r i -> Array (Html r i) -> SelectItem a r i
selectItem =
    SelectItem
