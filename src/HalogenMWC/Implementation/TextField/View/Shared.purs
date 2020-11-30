module HalogenMWC.Implementation.TextField.View.Shared where

import Protolude

import Data.Array as Array
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import Material.Classes.Textfield (mdc_floating_label, mdc_floating_label____float_above, mdc_floating_label____required, mdc_floating_label____shake, mdc_text_field, mdc_text_field____disabled, mdc_text_field____end_aligned, mdc_text_field____filled, mdc_text_field____focused, mdc_text_field____fullwidth, mdc_text_field____invalid, mdc_text_field____label_floating, mdc_text_field____ltr_text, mdc_text_field____no_label, mdc_text_field____outlined, mdc_text_field____textarea)

data LabelConfig
  = LabelConfig__With
    { id :: String -- for aria-label
    , labelText :: String
    }
  | LabelConfig__Without
    String -- labelText in props

textFieldLabelClasses ::
  { disabled :: Boolean
  , endAligned :: Boolean
  , filled :: Boolean
  , focused :: Boolean
  , fullwidth :: Boolean
  , invalid :: Boolean
  , labelFloating :: Boolean
  , ltrText :: Boolean
  , noLabel :: Boolean
  , outlined :: Boolean
  , textarea :: Boolean
  }
  -> Array ClassName
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

inputARIALabelProp :: forall t51 t52. LabelConfig -> Array (IProp t52 t51)
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
