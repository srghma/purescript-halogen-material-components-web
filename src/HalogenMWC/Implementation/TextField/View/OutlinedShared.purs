module HalogenMWC.Implementation.TextField.View.OutlinedShared where

import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.View.Shared

labelWrapper =
  HH.span
  [ HP.class_ mdc_notched_outline__notch
  ]

notchedOutlineLeadingElement = HH.span [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElement = HH.span [ HP.class_ mdc_notched_outline__trailing ] []

outlinedClasses = [ mdc_text_field, mdc_text_field____outlined ]

notchedOutlineElement config =
  HH.span
  [ HP.classes $ Array.catMaybes
    [ Just mdc_notched_outline
    , Just mdc_notched_outline____upgraded
    , if shouldFloat config then Just mdc_notched_outline____notched else Nothing
    ]
  ]
  ( Array.catMaybes
    [ Just notchedOutlineLeadingElement
    , case config.label of
           LabelConfig__With labelConfig -> Just $ labelWrapper [ labelElement config labelConfig ]
           LabelConfig__Without _ -> Nothing
    , Just notchedOutlineTrailingElement
    ]
  )
