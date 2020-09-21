module HalogenMWC.Implementation.TextField.Shared where

import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA

data LabelConfig
  = LabelConfig__With
    { id :: String
    , labelText :: String
    }
  | LabelConfig__Without String

noLabelClass :: LabelConfig -> Array ClassName
noLabelClass =
  case _ of
       LabelConfig__Without _ -> [ mdc_text_field____no_label ]
       LabelConfig__With _ -> []

inputLabelProp =
  case _ of
    LabelConfig__Without labelText -> [ HP.ARIA.label labelText ]
    LabelConfig__With labelConfig -> [ HP.ARIA.labelledBy labelConfig.id ]

labelElement labelConfig =
  HH.span
  [ HP.class_ mdc_floating_label
  , HP.id_ labelConfig.id
  ]
  [ HH.text labelConfig.labelText
  ]
