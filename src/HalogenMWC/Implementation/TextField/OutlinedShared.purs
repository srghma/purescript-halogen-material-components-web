module HalogenMWC.Implementation.TextField.OutlinedShared where

import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.Shared

labelWrapper =
  HH.span
  [ HP.class_ mdc_notched_outline__notch
  ]

notchedOutlineLeadingElement = HH.span [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElement = HH.span [ HP.class_ mdc_notched_outline__trailing ] []

outlinedClasses = [ mdc_text_field, mdc_text_field____outlined ]

notchedOutlineElement label =
  HH.span [ HP.class_ mdc_notched_outline ]
  ( Array.catMaybes
    [ Just notchedOutlineLeadingElement
    , case label of
           LabelConfig__With labelConfig -> Just $ labelWrapper [ labelElement labelConfig ]
           LabelConfig__Without _ -> Nothing
    , Just notchedOutlineTrailingElement
    ]
  )
