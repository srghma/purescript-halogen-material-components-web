module HalogenMWC.Implementation.Button.Insides where

import Halogen (AttrName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Button (mdc_button__icon, mdc_button__label)
import MaterialIconsFont.Classes (material_icons)

buttonLabel :: forall w i. String -> HH.HTML w i
buttonLabel = \label -> html [ HH.text label ]
  where
    html = HH.span [ HP.class_ mdc_button__label ]

buttonIconMaterialIcons :: forall w i. String -> HH.HTML w i
buttonIconMaterialIcons iconName = HH.i props [ HH.text iconName ]
  where
    props =
      [ HP.classes [ mdc_button__icon, material_icons ]
      , HP.attr (AttrName "aria-hidden") "true"
      ]
