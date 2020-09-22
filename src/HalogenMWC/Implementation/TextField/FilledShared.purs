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

-- TODO: implement
-- NOTE: only for filled, not outlined
data LineRippleState
  = LineRippleState__Idle
  | LineRippleState__Active -- active class is added, deactivating is removed
  | LineRippleState__Deactivating -- show active and deactivating classes, start listening for transision end, then go to idle

type LineRippleConfig
  = { xCoordinatePx :: Int
    , state :: LineRippleState
    }

xCoordinatePxRender int = styleVar "transform-origin" (show int <> "px center")

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
