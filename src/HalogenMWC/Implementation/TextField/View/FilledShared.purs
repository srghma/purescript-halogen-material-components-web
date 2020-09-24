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

lineRippleElement =
  case _ of
       FocusState__Idle ->
         HH.span
         [HP.class_ mdc_line_ripple]
         []
       FocusState__Active maybeXCoordinatePx ->
         HH.span
         ( Array.catMaybes $
           [ Just $ HP.classes [ mdc_line_ripple, mdc_line_ripple____active ]
           , map (HP.style <<< xCoordinatePxRender) maybeXCoordinatePx
           ]
         )
         []
  where
    xCoordinatePxRender :: Number -> String
    xCoordinatePxRender x = styleVar "transform-origin" (show x <> "px center")

filledClasses = [ mdc_text_field, mdc_text_field____filled ]

wrapInputElement :: ∀ t27 t28 t44. { label ∷ LabelConfig , focusState ∷ FocusState , required ∷ Boolean , shake ∷ Boolean , value ∷ String | t44 } → Array (HH.HTML t27 t28) → Array (HH.HTML t27 t28)
wrapInputElement config inputElement =
  [ rippleElement ]
  <>
  ( case config.label of
         LabelConfig__With labelConfig -> [ labelElement config labelConfig ]
         LabelConfig__Without _ -> []
  )
  <> inputElement <>
  [ lineRippleElement config.focusState ]
