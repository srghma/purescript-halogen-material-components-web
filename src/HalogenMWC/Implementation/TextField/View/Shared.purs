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
    { id :: String -- for aria-label
    , labelText :: String
    }
  | LabelConfig__Without
    String -- labelText in props

textFieldLabelClasses = \config ->
  Array.catMaybes
  [ Just mdc_text_field
  , if config.disabled      then Just mdc_text_field____disabled       else Nothing
  , if config.endAligned    then Just mdc_text_field____end_aligned    else Nothing
  , if config.filled        then Just mdc_text_field____filled         else Nothing
  , if config.focused       then Just mdc_text_field____focused        else Nothing
  , if config.fullwidth     then Just mdc_text_field____fullwidth      else Nothing
  , if config.invalid       then Just mdc_text_field____invalid        else Nothing
  , if config.labelFloating then Just mdc_text_field____label_floating else Nothing
  , if config.ltrText       then Just mdc_text_field____ltr_text       else Nothing
  , if config.noLabel       then Just mdc_text_field____no_label       else Nothing
  , if config.outlined      then Just mdc_text_field____outlined       else Nothing
  , if config.textarea      then Just mdc_text_field____textarea       else Nothing
  ]

isNoLabel :: LabelConfig -> Boolean
isNoLabel =
  case _ of
      LabelConfig__Without _ -> true
      _ -> false

inputARIALabelProp =
  case _ of
    LabelConfig__With labelConfig -> [ HP.ARIA.labelledBy labelConfig.id ] -- this is a link to floatingLabelSpanElement
    LabelConfig__Without labelText -> [ HP.ARIA.label labelText ]

isDirty :: String -> Boolean
isDirty x = x /= ""

floatingLabelSpanElement
  :: ∀ w i
   . { labelConfig ∷ { id :: String , labelText :: String }
     , required ∷ Boolean
     , shake ∷ Boolean
     , floatAbove :: Boolean
     }
   → HH.HTML w i
floatingLabelSpanElement = \config ->
  HH.span
    [ HP.classes $ Array.catMaybes
        [ Just mdc_floating_label
        , if config.floatAbove then Just mdc_floating_label____float_above else Nothing
        , if config.required then Just mdc_floating_label____required else Nothing
        , if config.shake then Just mdc_floating_label____shake else Nothing
        ]
    , HP.id_ config.labelConfig.id
    ]
    [ HH.text config.labelConfig.labelText
    ]
