module HalogenMWC.Implementation.TextField.FilledShared where

import HalogenMWC.Implementation.TextField.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Utils (styleVar)

rippleElement = HH.span [ HP.class_ mdc_text_field__ripple ] []

-- https://github.com/material-components/material-components-web/blob/a3212b2099765947f2a41d71af2cd95fcbca4b97/packages/mdc-line-ripple/foundation.ts#L68
-- | NOTE: only for filled, not outlined
data LineRippleState
  = LineRippleState__Idle
  | LineRippleState__Active Number -- active class is added, deactivating is removed
  -- | | LineRippleState__Deactivating -- show active and deactivating classes, start listening for transition end, then go to idle

lineRippleElement =
  case _ of
       LineRippleState__Idle ->
         HH.span
         [HP.class_ mdc_line_ripple]
         []
       LineRippleState__Active xCoordinatePx ->
         HH.span
         [ HP.classes [ mdc_line_ripple, mdc_line_ripple____active ]
         , HP.style $ xCoordinatePxRender xCoordinatePx
         ]
         []
  where
    xCoordinatePxRender :: Number -> String
    xCoordinatePxRender x = styleVar "transform-origin" (show x <> "px center")

filledClasses = [ mdc_text_field, mdc_text_field____filled ]

wrapInputElement { label, lineRippleState } inputElement =
  [ rippleElement ]
  <>
  ( case label of
         LabelConfig__With labelConfig -> [ labelElement labelConfig ]
         LabelConfig__Without _ -> []
  )
  <> inputElement <>
  [ lineRippleElement lineRippleState ]
