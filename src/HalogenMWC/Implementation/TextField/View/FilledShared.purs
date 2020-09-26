module HalogenMWC.Implementation.TextField.View.FilledShared where

import HalogenMWC.Implementation.TextField.View.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Utils (styleVar)
import HalogenMWC.Implementation.TextField.View.Shared

rippleElement = HH.span [ HP.class_ mdc_text_field__ripple ] []

lineRippleElement = \config ->
    HH.span
    [ HP.classes $ Array.catMaybes
      [ Just mdc_line_ripple
      , activeClass config
      ]
    ]
    []
  where
    activeClass config = if config.focused then Just mdc_line_ripple____active else Nothing

    -- TODO: it doesn't work anyway on official 7.0v https://curvy-expensive-cobra.glitch.me/

    -- | rippleCenterPoint =
    -- |   case _ of
    -- |        ActivationState__Active { rippleCenterPoint: Just rippleCenterPoint } -> Just $ HP.style $ styleVar "transform-origin" (show rippleCenterPoint <> "px center")
    -- |        _ -> Nothing

wrapInputElement :: ∀ t27 t28 t44. { label ∷ LabelConfig , focused ∷ Boolean, required ∷ Boolean , shake ∷ Boolean , value ∷ String | t44 } → Array (HH.HTML t27 t28) → Array (HH.HTML t27 t28)
wrapInputElement config inputElement =
  [ rippleElement ]
  <> case config.label of
         LabelConfig__With labelConfig ->
           Array.singleton $ floatingLabelSpanElement
             { floadAbove: config.focused || isDirty config.value
             , focused: config.focused
             , required: config.required
             , shake: config.shake
             ----
             , labelConfig
             }
         LabelConfig__Without _ -> []
  <> inputElement
  [ lineRippleElement config.activationState ]
