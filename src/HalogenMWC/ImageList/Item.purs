module HalogenMWC.ImageArray.Item
    ( Config, config



    , ImageArrayItem, imageArrayItem
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.ImageArray.Item.Internal (Config(..), ImageArrayItem(..))



type Config r i =
    Material.ImageArray.Item.Internal.Config r i



config :: Config r i
config =
    Config
        { label = Nothing
        , href = Nothing
        , additionalAttributes = []
        , image = ""
        }

















{-| Image list item type

Image list items can only be rendered within a [image list
container](Material-ImageArray)

-}
data ImageArrayItem r i =
    Material.ImageArray.Item.Internal.ImageArrayItem r i



imageArrayItem :: Config r i -> String -> ImageArrayItem r i
imageArrayItem (Config config_) image =
    ImageArrayItem (Config { config_ | image = image })
