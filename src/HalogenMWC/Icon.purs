module HalogenMWC.Icon (icon) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




icon :: Array (IProp r i) -> String -> Html r i
icon additionalAttributes iconName =
    Html.i (class "material-icons" :: additionalAttributes) [ text iconName ]
