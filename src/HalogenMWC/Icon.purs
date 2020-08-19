module HalogenMWC.Icon where

import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import MaterialIconsFont.Classes

icon :: Array (IProp r i) -> String -> HH.HTML w i
icon additionalAttributes iconName = HH.i ([ HP.class_ material_icons ] <> additionalAttributes) [ HH.text iconName ]
