module HalogenMWC.Implementation.TextField.View.Input where

import HalogenMWC.Implementation.TextField.View.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.String as String
import Halogen (AttrName(..), ClassName)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.View.CharacterCounter (CharacterCounterConfig)
import HalogenMWC.Implementation.TextField.View.CharacterCounter as CharacterCounter
import HalogenMWC.Implementation.TextField.View.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.View.HelperText (HelperTextConfig)
import HalogenMWC.Implementation.TextField.View.HelperText as HelperText
import HalogenMWC.Implementation.TextField.View.HelperTextAndCharacterCounter as HelperTextAndCharacterCounter
import HalogenMWC.Implementation.TextField.View.OutlinedShared as OutlinedShared

type ConfigAddedByComponentOnRender i r =
  ( additionalAttributesInput :: Array (IProp I.HTMLinput i)
  | r
  )

data CharacterCounterOrMaxLength
  = CharacterCounterOrMaxLength__All_Disabled
  | CharacterCounterOrMaxLength__Only_MaxLength Int
  | CharacterCounterOrMaxLength__Enabled Int

type ConfigManagedByUser i r =
  ( label       :: LabelConfig
  , placeholder :: Maybe String
  , type_       :: InputType

  , disabled  :: Boolean
  , fullwidth :: Boolean
  , invalid   :: Boolean

  , helperText                  :: Maybe HelperTextConfig
  , characterCounterOrMaxLength :: CharacterCounterOrMaxLength

  , minLength                 :: Maybe Int
  , prefix                    :: Maybe String
  , suffix                    :: Maybe String

  , additionalAttributesRoot :: Array (IProp I.HTMLdiv i)

  , value :: String
  , shake :: Boolean
  , required :: Boolean
  , endAligned :: Boolean
  , ltrText :: Boolean

  | r
  )

type ConfigManagedByComponent r =
  ( focused :: Boolean
  | r
  )

type Config i = Record (ConfigManagedByUser i + ConfigManagedByComponent + ConfigAddedByComponentOnRender i + ())

inputElement
  :: ∀ i w
  . Record (ConfigManagedByUser i + ConfigManagedByComponent + ConfigAddedByComponentOnRender i + ())
  → HH.HTML w i
inputElement config =
  HH.input
  (
    [ HP.classes $ [ mdc_text_field__input ]
    , HP.type_ config.type_
    , HP.disabled config.disabled
    , HP.value config.value
    ]
    <> Array.catMaybes
      [ map (HP.attr (AttrName "minLength") <<< show) config.minLength
      , map (HP.attr (AttrName "maxLength") <<< show) $
        case config.characterCounterOrMaxLength of
             CharacterCounterOrMaxLength__Only_MaxLength int -> Just int
             CharacterCounterOrMaxLength__Enabled int -> Just int
             _ -> Nothing
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

configToRenderBoth
  :: forall i w
   . Config i
  -> { helperText       :: Maybe HelperTextConfig
     , characterCounter :: Maybe CharacterCounterConfig
     }
configToRenderBoth config =
  { helperText: config.helperText
  , characterCounter:
      case config.characterCounterOrMaxLength of
           CharacterCounterOrMaxLength__Enabled max ->
             Just
             { value: String.length config.value
             , max
             }
           _ -> Nothing
  }

renderInternalAndBoth renderInternal = \config -> HH.div config.additionalAttributesRoot $ [ renderInternal config ] <> HelperTextAndCharacterCounter.renderBoth (configToRenderBoth config)

filled :: forall w i . Config i -> HH.HTML w i
filled = renderInternalAndBoth renderInternal
  where
    renderInternal :: Config i -> HH.HTML w i
    renderInternal = \config ->
      let
        labelFloating = config.focused || isDirty config.value
      in
      HH.label
      ( [ HP.classes $
          textFieldLabelClasses
          { disabled:      config.disabled
          , endAligned:    config.endAligned
          , filled:        true
          , focused:       config.focused
          , fullwidth:     config.fullwidth
          , invalid:       config.invalid
          , labelFloating
          , ltrText:       config.ltrText
          , noLabel:       isNoLabel config.label
          , outlined:      false
          , textarea:      false
          }
        ]
      )
      ( FilledShared.wrapInputElement
        { floatAbove: labelFloating
        , focused:    config.focused
        , label:      config.label
        , required:   config.required
        , shake:      config.shake
        }
        ( Array.catMaybes
          [ maybePrefixElement config.prefix
          , Just $ inputElement config
          , maybeSuffixElement config.suffix
          ]
        )
      )

outlined :: forall w i . Config i -> HH.HTML w i
outlined = renderInternalAndBoth renderInternal
  where
    renderInternal :: Config i -> HH.HTML w i
    renderInternal = \config ->
      let
        labelFloating = config.focused || isDirty config.value
        noLabel = isNoLabel config.label
      in
        HH.label
        ( [ HP.classes $
            textFieldLabelClasses
            { disabled:      config.disabled
            , endAligned:    config.endAligned
            , filled:        false
            , focused:       config.focused
            , fullwidth:     config.fullwidth
            , invalid:       config.invalid -- TODO: when required AND was touched (https://curvy-expensive-cobra.glitch.me/)
            , labelFloating
            , ltrText:       config.ltrText
            , noLabel
            , outlined:      true
            , textarea:      false
            }
          ]
        )
        ( Array.catMaybes
          [ maybePrefixElement config.prefix
          , Just $ inputElement config
          , maybeSuffixElement config.suffix
          , Just $ OutlinedShared.notchedOutlineElement
              { noLabel
              , floatAbove: labelFloating
              , label:      config.label
              , required:   config.required
              , shake:      config.shake
              }
          ]
        )
