module HalogenMWC.Implementation.TextField.Input where

import HalogenMWC.Implementation.TextField.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Halogen (AttrName(..), ClassName)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.CharacterCounter as CharacterCounter
import HalogenMWC.Implementation.TextField.CharacterCounter (CharacterCounterConfig)
import HalogenMWC.Implementation.TextField.HelperText (HelperTextConfig)
import HalogenMWC.Implementation.TextField.HelperText as HelperText
import HalogenMWC.Implementation.TextField.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.OutlinedShared as OutlinedShared
import HalogenMWC.Implementation.TextField.HelperTextAndCharacterCounter as HelperTextAndCharacterCounter

type ConfigManagedByUser r =
  ( label       :: LabelConfig
  , placeholder :: Maybe String
  , type_       :: InputType

  , disabled  :: Boolean
  , fullwidth :: Boolean
  , invalid   :: Boolean

  , helperText                :: Maybe HelperTextConfig
  , characterCounter          :: Maybe CharacterCounterConfig

  -- | , maxLength                 :: Maybe Int

  , minLength                 :: Maybe Int
  , prefix                    :: Maybe String
  , suffix                    :: Maybe String

  , additionalClassesRoot     :: Array ClassName
  , additionalClassesInput    :: Array ClassName
  | r )

type ConfigManagedByComponent r =
  ( focused                   :: Boolean
  , lineRippleState           :: FilledShared.LineRippleState
  | r )

type ConfigAddedByComponent i r =
  ( additionalAttributesRoot  :: Array (IProp I.HTMLlabel i)
  , additionalAttributesInput :: Array (IProp I.HTMLinput i)
  | r )

type Config i = Record (ConfigManagedByUser + ConfigManagedByComponent + ConfigAddedByComponent i + ())

inputElement config =
  HH.input
  (
    [ HP.classes $ [ mdc_text_field__input ] <> config.additionalClassesInput
    , HP.type_ config.type_
    , HP.disabled config.disabled
    ]
    <> Array.catMaybes
      [ map (HP.attr (AttrName "minLength") <<< show) config.minLength
      , map (HP.attr (AttrName "maxLength") <<< show <<< _.max) config.characterCounter
      ]
    <> inputARIALabelProp config.label
    <> HelperText.maybeInputProps config.helperText
    <> config.additionalAttributesInput
  )

prefixElement :: ∀ t6 t7. Array (HH.HTML t7 t6) → HH.HTML t7 t6
prefixElement = HH.span [ HP.classes [ mdc_text_field__affix, mdc_text_field__affix____prefix ] ]

suffixElement :: ∀ t1 t2. Array (HH.HTML t2 t1) → HH.HTML t2 t1
suffixElement = HH.span [ HP.classes [ mdc_text_field__affix, mdc_text_field__affix____suffix ] ]

maybePrefixElement :: ∀ t19 t22 t23. Functor t19 ⇒ t19 String → t19 (HH.HTML t22 t23)
maybePrefixElement = map \s -> prefixElement [ HH.text s ]

maybeSuffixElement :: ∀ t11 t14 t15. Functor t11 ⇒ t11 String → t11 (HH.HTML t14 t15)
maybeSuffixElement = map \s -> suffixElement [ HH.text s ]

-------------------------

filled :: forall w i . Config i -> Array (HH.HTML w i)
filled = HelperTextAndCharacterCounter.wrapRenderBoth \config ->
  HH.label
  ( [ HP.classes $ FilledShared.filledClasses <> rootLabelClasses config <> config.additionalClassesRoot
    ] <> config.additionalAttributesRoot
  )
  ( FilledShared.wrapInputElement config $
    Array.catMaybes
    [ maybePrefixElement config.prefix
    , Just $ inputElement config
    , maybeSuffixElement config.suffix
    ]
  )

outlined :: forall w i . Config i -> Array (HH.HTML w i)
outlined = HelperTextAndCharacterCounter.wrapRenderBoth \config ->
  HH.label [ HP.classes $ OutlinedShared.outlinedClasses <> rootLabelClasses config ]
  ( Array.catMaybes
    [ maybePrefixElement config.prefix
    , Just $ inputElement config
    , maybeSuffixElement config.suffix
    , Just $ OutlinedShared.notchedOutlineElement config.label
    ]
  )
