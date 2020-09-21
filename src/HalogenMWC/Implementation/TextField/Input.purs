module HalogenMWC.Implementation.TextField.Input where

import Protolude

import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Button (mdc_button, mdc_button____outlined, mdc_button____raised, mdc_button____touch, mdc_button____unelevated, mdc_button__ripple, mdc_button__touch, mdc_touch_target_wrapper)
import Material.Classes.Textfield

html :: forall w i . HH.HTML w i
html =
  HH.label
  [ HP.classes [mdc_text_field, mdc_text_field____filled]
  ]
  [ HH.span [HP.class_ mdc_text_field__ripple] []
  , HH.span
    [ HP.class_ mdc_floating_label
    , HP.id "my-label-id"
    ]
    [ HH.text "Hint text"
    ]
  , HH.input
    [ HP.class_ mdc_text_field__input
    , HP.type_ "text"
    , HP.aria-labelledby "my-label-id"
    ]
  , HH.span [HP.class_ mdc_line_ripple] []
  ]
