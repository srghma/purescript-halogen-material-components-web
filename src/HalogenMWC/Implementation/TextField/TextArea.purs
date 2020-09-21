module HalogenMWC.Implementation.TextField.TextArea where

import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.Shared
import HalogenMWC.Implementation.TextField.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.OutlinedShared as OutlinedShared

type Config =
  { label :: LabelConfig
  , rows :: Int
  , cols :: Int
  , resizable :: Boolean
  }

resizerClass :: Boolean -> Array ClassName
resizerClass = if _ then [ mdc_text_field__resizer ] else []

-------------------------

inputElement config =
  HH.span
  [ HP.classes (resizerClass config.resizable)
  ]
  [ HH.textarea $
    [ HP.class_ mdc_text_field__input
    , HP.rows config.rows
    , HP.cols config.cols
    ] <> inputLabelProp config.label
  ]

textareaClasses = [ mdc_text_field____textarea ]

filled :: forall w i . Config -> HH.HTML w i
filled config =
  HH.label
  [ HP.classes $ FilledShared.filledClasses <> textareaClasses <> noLabelClass config.label ]
  (FilledShared.wrapInputElement config.label $ inputElement config)

outlined :: forall w i . Config -> HH.HTML w i
outlined config =
  HH.label
  [ HP.classes $ OutlinedShared.outlinedClasses <> textareaClasses <> noLabelClass config.label ]
  [ inputElement config
  , OutlinedShared.notchedOutlineElement config.label
  ]
