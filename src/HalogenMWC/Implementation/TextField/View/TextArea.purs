module HalogenMWC.Implementation.TextField.View.TextArea where

import HalogenMWC.Implementation.TextField.View.Shared (LabelConfig, inputARIALabelProp)
import Material.Classes.Textfield (mdc_text_field____textarea, mdc_text_field____with_internal_counter, mdc_text_field__input, mdc_text_field__resizer)
import Protolude

import Data.Array as Array
import Halogen (AttrName(..), ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Implementation.TextField.View.HelperText as HelperText
import HalogenMWC.Implementation.TextField.View.CharacterCounter as CharacterCounter

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

textareaClasses :: Array ClassName
textareaClasses = [ mdc_text_field____textarea ]

internalCounterClass :: Maybe CharacterCounterOrMaxLengthType -> Array ClassName
internalCounterClass =
  case _ of
    Just (CharacterCounterType__Internal _) -> [ mdc_text_field____with_internal_counter ]
    _ -> []

-------------------------

inputElement
  :: forall i w r t81.
  { cols :: Int
  , disabled :: Boolean
  , helperTextId :: Maybe { id :: String | r }
  , internalCounterOrMaxLength :: Maybe CharacterCounterOrMaxLengthType
  , label :: LabelConfig
  , minLength :: Maybe Int
  , resizable :: Boolean
  , rows :: Int
  | t81
  }
  -> HH.HTML w i
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


-- | filled :: forall w i . Config -> HH.HTML w i
-- | filled config =
-- |   HH.label
-- |   [ HP.classes $ FilledShared.filledClasses <> textareaClasses <> rootLabelClasses config <> internalCounterClass config.internalCounterOrMaxLength ]
-- |   (FilledShared.wrapInputElement config.label [ inputElement config ])

-- | outlined :: forall w i . Config -> HH.HTML w i
-- | outlined config =
-- |   HH.label
-- |   [ HP.classes $ OutlinedShared.outlinedClasses <> textareaClasses <> rootLabelClasses config <> internalCounterClass config.internalCounterOrMaxLength ]
-- |   [ inputElement config
-- |   , OutlinedShared.notchedOutlineElement config.label
-- |   ]
