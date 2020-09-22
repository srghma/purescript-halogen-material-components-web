module HalogenMWC.Implementation.TextField.TextArea where

import HalogenMWC.Implementation.TextField.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (AttrName(..), ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.HelperText as HelperText
import HalogenMWC.Implementation.TextField.OutlinedShared as OutlinedShared
import HalogenMWC.Implementation.TextField.CharacterCounter as CharacterCounter

data CharacterCounterOrMaxLengthType
  = CharacterCounterType__External_or_MaxLength Int -- maxLength OR the `_.max` from CharacterCounterConfig
  | CharacterCounterType__Internal CharacterCounter.CharacterCounterConfig

type Config =
  { label :: LabelConfig
  , rows :: Int
  , cols :: Int
  , resizable :: Boolean

  , disabled  :: Boolean
  , focused   :: Boolean
  , fullwidth :: Boolean
  , invalid   :: Boolean

  , helperTextId :: Maybe String
  , minLength :: Maybe Int

  -- only when want to render internally
  , internalCounterOrMaxLength :: Maybe CharacterCounterOrMaxLengthType
  }

characterCounterOrMaxLength :: CharacterCounterOrMaxLengthType -> Int
characterCounterOrMaxLength =
  case _ of
       CharacterCounterType__External_or_MaxLength maxLength -> maxLength
       CharacterCounterType__Internal config -> config.max

resizerClass :: Boolean -> Array ClassName
resizerClass = if _ then [ mdc_text_field__resizer ] else []

textareaClasses = [ mdc_text_field____textarea ]

internalCounterClass =
  case _ of
    Just (CharacterCounterType__Internal _) -> [ mdc_text_field____with_internal_counter ]
    _ -> []

-------------------------

inputElement = \config ->
  HH.span
  [ HP.classes (resizerClass config.resizable)
  ]
  ( [ HH.textarea $
      [ HP.class_ mdc_text_field__input
      , HP.rows config.rows
      , HP.cols config.cols
      , HP.disabled config.disabled
      ]
      <> Array.catMaybes
        [ map (HP.attr (AttrName "minLength") <<< show) config.minLength
        , map (HP.attr (AttrName "maxLength") <<< show <<< characterCounterOrMaxLength) $ config.internalCounterOrMaxLength
        ]
      <> inputARIALabelProp config.label
      <> HelperText.maybeInputProps config.helperTextId
    ]
    <> case config.internalCounterOrMaxLength of
            Just (CharacterCounterType__Internal counterConfig) -> [ CharacterCounter.characterCounter counterConfig ]
            _ -> []
  )


filled :: forall w i . Config -> HH.HTML w i
filled config =
  HH.label
  [ HP.classes $ FilledShared.filledClasses <> textareaClasses <> rootLabelClasses config <> internalCounterClass config.internalCounterOrMaxLength ]
  (FilledShared.wrapInputElement config.label [ inputElement config ])

outlined :: forall w i . Config -> HH.HTML w i
outlined config =
  HH.label
  [ HP.classes $ OutlinedShared.outlinedClasses <> textareaClasses <> rootLabelClasses config <> internalCounterClass config.internalCounterOrMaxLength ]
  [ inputElement config
  , OutlinedShared.notchedOutlineElement config.label
  ]
