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

-- https://github.com/material-components/material-components-web/blob/a3212b2099765947f2a41d71af2cd95fcbca4b97/packages/mdc-line-ripple/foundation.ts#L68
data ActivationState
  = ActivationState__Idle
  | ActivationState__Active
    Boolean -- focused
    -- | { focused :: Boolean
    -- | , rippleCenterPoint :: Maybe Number
    -- | }

isFocused =
  case _ of
       ActivationState__Active focused -> focused
       _ -> false

isActive =
  case _ of
       ActivationState__Active _ -> true
       _ -> false

rippleElement = HH.span [ HP.class_ mdc_text_field__ripple ] []

lineRippleElement = \activationState ->
    HH.span
    [ HP.classes $ Array.catMaybes
      [ Just mdc_line_ripple
      , activeClass activationState
      ]
      -- | , rippleCenterPoint activationState
    ]
    []
  where
    activeClass activationState = if isActive activationState then Just mdc_line_ripple____active else Nothing

    -- TODO: it doesn't work anyway on official 7.0v https://curvy-expensive-cobra.glitch.me/

    -- | rippleCenterPoint =
    -- |   case _ of
    -- |        ActivationState__Active { rippleCenterPoint: Just rippleCenterPoint } -> Just $ HP.style $ styleVar "transform-origin" (show rippleCenterPoint <> "px center")
    -- |        _ -> Nothing

filledClasses = [ mdc_text_field, mdc_text_field____filled ]

wrapInputElement :: ∀ t27 t28 t44. { label ∷ LabelConfig , activationState ∷ ActivationState , required ∷ Boolean , shake ∷ Boolean , value ∷ String | t44 } → Array (HH.HTML t27 t28) → Array (HH.HTML t27 t28)
wrapInputElement config inputElement =
  [ rippleElement ]
  <>
  ( case config.label of
         LabelConfig__With labelConfig -> [ labelElement isActive config labelConfig ]
         LabelConfig__Without _ -> []
  )
  <> inputElement <>
  [ lineRippleElement config.activationState ]
