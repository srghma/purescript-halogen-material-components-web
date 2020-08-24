module HalogenMWC.Icon where

import MaterialIconsFont.Classes (material_icons)
import Protolude ((<>))

import DOM.HTML.Indexed (HTMLi) as I
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

icon :: forall i w . Array (IProp I.HTMLi i) -> String -> HH.HTML w i
icon additionalAttributes iconName = HH.i ([ HP.class_ material_icons ] <> additionalAttributes) [ HH.text iconName ]
