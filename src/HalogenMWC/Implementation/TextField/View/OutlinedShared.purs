module HalogenMWC.Implementation.TextField.View.OutlinedShared where

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

data ActivationState
  = ActivationState__Idle
  | ActivationState__Active
    { focused :: Boolean
    , notchWidth :: Int
    }

isFocused =
  case _ of
       ActivationState__Active { focused } -> focused
       _ -> false

isActive =
  case _ of
       ActivationState__Active _ -> true
       _ -> false

outlineNotchWrapper = \activationState ->
  HH.span $ Array.catMaybes
  [ Just $ HP.class_ mdc_notched_outline__notch
  , notchWidth activationState
  ]
  where
    notchWidth =
      case _ of
           ActivationState__Idle -> Nothing
           ActivationState__Active { notchWidth } -> Just $ HP.style $ styleVar "width" (show notchWidth <> "px")

notchedOutlineLeadingElement = HH.span [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElement = HH.span [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineElement config =
  HH.span
  [ HP.classes $ Array.catMaybes
    [ Just mdc_notched_outline
    , Just mdc_notched_outline____upgraded
    , if shouldFloat isActive config then Just mdc_notched_outline____notched else Nothing
    ]
  ]
  ( Array.catMaybes
    [ Just notchedOutlineLeadingElement
    , case config.label of
           LabelConfig__With labelConfig -> Just $ outlineNotchWrapper config.activationState [ labelElement isActive config labelConfig ]
           LabelConfig__Without _ -> Nothing
    , Just notchedOutlineTrailingElement
    ]
  )
