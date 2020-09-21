module HalogenMWC.Implementation.TextField.Input where

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
  , placeholder :: Maybe String
  , type_ :: InputType
  }

inputElement config =
  HH.input
  (
    [ HP.class_ mdc_text_field__input
    , HP.type_ config.type_
    ]
    <> inputLabelProp config.label
  )

-------------------------

filled :: forall w i . Config -> HH.HTML w i
filled = \config ->
  HH.label
  [ HP.classes $ FilledShared.filledClasses <> noLabelClass config.label ]
  (FilledShared.wrapInputElement config.label $ inputElement config)

outlined :: forall w i . Config -> HH.HTML w i
outlined = \config ->
  HH.label [ HP.classes $ OutlinedShared.outlinedClasses <> noLabelClass config.label ]
  [ inputElement config
  , OutlinedShared.notchedOutlineElement config.label
  ]
