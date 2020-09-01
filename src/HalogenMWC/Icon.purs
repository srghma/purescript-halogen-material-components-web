module HalogenMWC.Icon where

import MaterialIconsFont.Classes (material_icons)
import Prelude
import DOM.HTML.Indexed (HTMLi) as I
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

materialIcon :: forall w i. Array (IProp I.HTMLi i) -> String -> HH.HTML w i
materialIcon additionalAttributes iconName = HH.i ([ HP.class_ material_icons ] <> additionalAttributes) [ HH.text iconName ]
