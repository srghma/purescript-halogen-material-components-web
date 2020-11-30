module HalogenMWC.Implementation.TextField.View.FilledShared where

import HalogenMWC.Implementation.TextField.View.Shared (LabelConfig(..), floatingLabelSpanElement)
import Material.Classes.Textfield (mdc_line_ripple, mdc_line_ripple____active, mdc_text_field__ripple)
import Protolude

import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Implementation.TextField.View.Shared (LabelConfig(..), floatingLabelSpanElement)

wrapInputElement :: ∀ t2 t3 t42. { floatAbove ∷ Boolean , focused ∷ Boolean , label ∷ LabelConfig , required ∷ Boolean , shake ∷ Boolean | t42 } → Array (HH.HTML t3 t2) → Array (HH.HTML t3 t2)
wrapInputElement = \config inputElement ->
  rippleElement
  <> case config.label of
         LabelConfig__With labelConfig ->
           Array.singleton $ floatingLabelSpanElement
             { labelConfig
             , floatAbove: config.floatAbove
             , required: config.required
             , shake: config.shake
             }
         LabelConfig__Without _ -> []
  <> inputElement
  <> [ lineRippleElement config ]
  where
    rippleElement = [ HH.span [ HP.class_ mdc_text_field__ripple ] [] ]

    lineRippleElement = \config ->
      HH.span
      [ HP.classes $ Array.catMaybes
        [ Just mdc_line_ripple
        , if config.focused then Just mdc_line_ripple____active else Nothing
        ]
      ]
      []
