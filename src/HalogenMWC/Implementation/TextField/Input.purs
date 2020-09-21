module HalogenMWC.Implementation.TextField.Input where

import HalogenMWC.Implementation.TextField.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Halogen (AttrName(..), ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.CharacterCounter as CharacterCounter
import HalogenMWC.Implementation.TextField.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.HelperText as HelperText
import HalogenMWC.Implementation.TextField.OutlinedShared as OutlinedShared

type Config =
  { label :: LabelConfig
  , placeholder :: Maybe String
  , type_ :: InputType
  , disabled :: Boolean
  , helperTextId :: Maybe String
  , maxLength :: Maybe Int
  , minLength :: Maybe Int
  , prefix :: Maybe String
  , suffix :: Maybe String
  }

inputElement config =
  HH.input
  (
    [ HP.class_ mdc_text_field__input
    , HP.type_ config.type_
    , HP.disabled config.disabled
    ]
    <> Array.catMaybes
      [ map (HP.attr (AttrName "minLength") <<< show) config.minLength
      , map (HP.attr (AttrName "maxLength") <<< show) config.maxLength
      ]
    <> inputLabelProp config.label
    <> HelperText.maybeInputProps config.helperTextId
  )

prefixElement = HH.span [ HP.classes [ mdc_text_field__affix, mdc_text_field__affix____prefix ] ]

suffixElement = HH.span [ HP.classes [ mdc_text_field__affix, mdc_text_field__affix____suffix ] ]

maybePrefixElement = map \s -> prefixElement [ HH.text s ]

maybeSuffixElement = map \s -> suffixElement [ HH.text s ]

-------------------------

filled :: forall w i . Config -> HH.HTML w i
filled = \config ->
  HH.label
  [ HP.classes $ FilledShared.filledClasses <> rootLabelClasses config ]
  ( FilledShared.wrapInputElement config.label $
    Array.catMaybes
    [ maybePrefixElement config.prefix
    , Just $ inputElement config
    , maybeSuffixElement config.suffix
    ]
  )

outlined :: forall w i . Config -> HH.HTML w i
outlined = \config ->
  HH.label [ HP.classes $ OutlinedShared.outlinedClasses <> rootLabelClasses config ]
  ( Array.catMaybes
    [ maybePrefixElement config.prefix
    , Just $ inputElement config
    , maybeSuffixElement config.suffix
    , Just $ OutlinedShared.notchedOutlineElement config.label
    ]
  )
