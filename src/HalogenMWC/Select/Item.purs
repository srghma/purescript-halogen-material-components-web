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
import HalogenMWC.Select.Item.Internal (Config(..), SelectItem(..))



data Config a r i =
    Material.Select.Item.Internal.Config a r i



config :: { value :: a } -> Config a r i
config { value } =
    Config
        { value = value
        , disabled = False
        , additionalAttributes = []
        }



setDisabled :: Boolean -> Config a r i -> Config a r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }



setAttributes :: Array (IProp r i) -> Config a r i -> Config a r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



data SelectItem a r i =
    Material.Select.Item.Internal.SelectItem a r i



selectItem :: Config a r i -> Array (Html r i) -> SelectItem a r i
selectItem =
    SelectItem
