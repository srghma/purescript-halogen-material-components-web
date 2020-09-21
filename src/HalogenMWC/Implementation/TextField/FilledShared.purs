module HalogenMWC.Implementation.TextField.FilledShared where

import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.Shared

rippleElement = HH.span [ HP.class_ mdc_text_field__ripple ] []

lineRippleElement = HH.span [HP.class_ mdc_line_ripple] []

filledClasses = [ mdc_text_field, mdc_text_field____filled ]

wrapInputElement label inputElement =
  [ rippleElement ]
  <>
  ( case label of
         LabelConfig__With labelConfig -> [ labelElement labelConfig ]
         LabelConfig__Without _ -> []
  )
  <> inputElement <> [ lineRippleElement ]
