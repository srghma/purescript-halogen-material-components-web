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
  | LabelConfig__Without
    String -- labelText in props

rootLabelClasses = \config ->
  Array.catMaybes
  [ noLabelClass       config.label
  , disabledClass      config.disabled
  , focusedClass       config.focused
  , fullwidthClass     config.fullwidth
  , invalidClass       config.invalid
  -- | , labelFloatingClass config.labelFloating
  ]
  where
    noLabelClass :: LabelConfig -> Maybe ClassName
    noLabelClass =
      case _ of
          LabelConfig__Without _ -> Just mdc_text_field____no_label
          LabelConfig__With _ -> Nothing

    disabledClass         = if _ then Just mdc_text_field____disabled else Nothing
    focusedClass          = if _ then Just mdc_text_field____focused else Nothing
    fullwidthClass        = if _ then Just mdc_text_field____fullwidth else Nothing
    invalidClass          = if _ then Just mdc_text_field____invalid else Nothing
    -- | labelFloatingClass    = if _ then Just mdc_text_field____label_floating else Nothing
    -- | withLeadingIconClass  = if _ then Just mdc_text_field____with_leading_icon else Nothing
    -- | withTrailingIconClass = if _ then Just mdc_text_field____with_trailing_icon else Nothing

inputARIALabelProp =
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
