module HalogenMWC.TextField where

import Material.Classes.LineRipple (mdc_line_ripple)
import Material.Classes.Textfield (mdc_floating_label, mdc_floating_label____float_above, mdc_notched_outline, mdc_notched_outline__leading, mdc_notched_outline__notch, mdc_notched_outline__trailing, mdc_text_field, mdc_text_field____disabled, mdc_text_field____fullwidth, mdc_text_field____no_label, mdc_text_field____outlined, mdc_text_field____with_leading_icon, mdc_text_field____with_trailing_icon, mdc_text_field__icon, mdc_text_field__input)
import Prelude
import Data.Maybe (Maybe(..), maybe)
import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..), ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Icon as Icon
import HalogenMWC.Utils as Utils

type Config w i
  = { label :: Maybe String
    , fullwidth :: Boolean
    , value :: Maybe String
    , placeholder :: Maybe String
    , disabled :: Boolean
    , required :: Boolean
    , valid :: Boolean
    , minLength :: Maybe Int
    , maxLength :: Maybe Int
    , pattern :: Maybe String
    , type_ :: Maybe InputType
    , min :: Maybe Int
    , max :: Maybe Int
    , step :: Maybe Int
    , leadingIcon :: Maybe (HH.HTML w i)
    , trailingIcon :: Maybe (HH.HTML w i)
    , additionalAttributes :: Array (IProp I.HTMLinput i)
    , onInput :: Maybe (String -> i)
    , onChange :: Maybe (String -> i)
    }

defaultConfig :: forall w i. Config w i
defaultConfig =
  { label: Nothing
  , fullwidth: false
  , value: Nothing
  , placeholder: Nothing
  , disabled: false
  , required: false
  , valid: true
  , minLength: Nothing
  , maxLength: Nothing
  , pattern: Nothing
  , type_: Nothing
  , min: Nothing
  , max: Nothing
  , step: Nothing
  , leadingIcon: Nothing
  , trailingIcon: Nothing
  , additionalAttributes: []
  , onInput: Nothing
  , onChange: Nothing
  }

data TextFieldType
  = Filled
  | Outlined

filled :: forall w i. Config w i -> HH.HTML w i
filled = textField Filled

outlined :: forall w i. Config w i -> HH.HTML w i
outlined = textField Outlined

textField :: forall w i. TextFieldType -> Config w i -> HH.HTML w i
textField type_ config =
  HH.element (ElemName "mdc-text-field")
    ( [ HP.classes
          ( Array.concat
              [ [ mdc_text_field ]
              , if config.fullwidth then [ mdc_text_field____fullwidth ] else []
              , if config.disabled then [ mdc_text_field____disabled ] else []
              ]
              <> Array.catMaybes
                  [ map (const mdc_text_field____no_label) config.label
                  , map (const mdc_text_field____outlined) config.type_
                  , map (const mdc_text_field____with_leading_icon) config.leadingIcon
                  , map (const mdc_text_field____with_trailing_icon) config.trailingIcon
                  ]
          )
      , HP.disabled config.disabled
      , HP.prop (PropName "required") config.required
      , HP.prop (PropName "valid") config.valid
      , HP.prop (PropName "minLength") (Maybe.fromMaybe (-1) config.minLength)
      , HP.prop (PropName "maxLength") (Maybe.fromMaybe (-1) config.maxLength)
      , HP.prop (PropName "min") (maybe "" show config.min)
      , HP.prop (PropName "max") (maybe "" show config.max)
      , HP.prop (PropName "step") (maybe "" show config.step)
      ]
        <> Array.catMaybes
            [ map (HP.prop (PropName "pattern")) config.pattern
            , map (HP.prop (PropName "value")) config.value
            ]
        <> config.additionalAttributes
    )
    ( Array.concat
        [ case config.leadingIcon of
            Nothing -> []
            Just html -> [ html ]
        , if config.fullwidth then case type_ of
            Filled ->
              [ inputElt config
              , lineRippleElt
              ]
            Outlined ->
              [ inputElt config
              , notchedOutlineElt config
              ]
          else case type_ of
            Filled ->
              [ inputElt config
              , labelElt config
              , lineRippleElt
              ]
            Outlined ->
              [ inputElt config
              , notchedOutlineElt config
              ]
        , case config.trailingIcon of
            Nothing -> []
            Just html -> [ html ]
        ]
    )

icon :: forall w i. Array (IProp I.HTMLi i) -> String -> HH.HTML w i
icon additionalAttributes iconName = Icon.icon ([ HP.class_ mdc_text_field__icon ] <> additionalAttributes) iconName

-----------------
inputElt :: forall w i. Config w i -> HH.HTML w i
inputElt config =
  HH.input
    ( [ HP.class_ mdc_text_field__input ]
        <> Array.catMaybes
            [ map HP.type_ config.type_
            , ariaLabelAttr config
            , map HP.placeholder config.placeholder
            , map HE.onValueInput config.onInput
            , map HE.onValueChange config.onChange
            , map (HP.attr (AttrName "minLength") <<< show) config.minLength
            , map (HP.attr (AttrName "maxLength") <<< show) config.maxLength
            ]
    )

ariaLabelAttr :: forall w i r. Config w i -> Maybe (IProp r i)
ariaLabelAttr config =
  if config.fullwidth then
    map (HP.attr (AttrName "aria-label")) config.label
  else
    Nothing

labelElt :: forall w i. Config w i -> HH.HTML w i
labelElt config = case config.label of
  Just str ->
    HH.div
      [ if Maybe.fromMaybe "" config.value /= "" then
          HP.classes [ mdc_floating_label, mdc_floating_label____float_above ]
        else
          HP.class_ mdc_floating_label
      , Utils.prop (PropName "foucClassNames") (Utils.propFromArrayClassName [ mdc_floating_label____float_above ])
      ]
      [ HH.text str ]
  Nothing -> HH.text ""

lineRippleElt :: forall w i. HH.HTML w i
lineRippleElt = HH.div [ HP.class_ mdc_line_ripple ] []

notchedOutlineElt :: forall w i. Config w i -> HH.HTML w i
notchedOutlineElt config =
  HH.div
    [ HP.class_ mdc_notched_outline ]
    [ notchedOutlineLeadingElt
    , notchedOutlineNotchElt config
    , notchedOutlineTrailingElt
    ]

notchedOutlineLeadingElt :: forall w i. HH.HTML w i
notchedOutlineLeadingElt = HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElt :: forall w i. HH.HTML w i
notchedOutlineTrailingElt = HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineNotchElt :: forall w i. Config w i -> HH.HTML w i
notchedOutlineNotchElt config = HH.div [ HP.class_ mdc_notched_outline__notch ] [ labelElt config ]
