module HalogenMWC.Implementation.TextField.View.Shared where

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

rootLabelClasses = \isFocused config ->
  Array.catMaybes
  [ noLabelClass       config.label
  , disabledClass      config.disabled
  , focusedClass       (isFocused config.activationState)
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

isDirty :: String -> Boolean
isDirty x = x /= ""

shouldFloat isActive config = isActive config.activationState || isDirty config.value

labelClasses :: ∀ t21 activationState. (activationState -> Boolean) -> { activationState ∷ activationState , required ∷ Boolean , shake ∷ Boolean , value ∷ String | t21 } → Array ClassName
labelClasses isActive = \config ->
  Array.catMaybes
    [ Just mdc_floating_label
    , if shouldFloat isActive config then Just mdc_floating_label____float_above else Nothing
    , if config.required then Just mdc_floating_label____required else Nothing
    , if config.shake then Just mdc_floating_label____shake else Nothing
    ]

labelElement isActive config labelConfig =
  HH.span
    [ HP.classes $ labelClasses isActive config
    , HP.id_ labelConfig.id
    ]
    [ HH.text labelConfig.labelText
    ]
