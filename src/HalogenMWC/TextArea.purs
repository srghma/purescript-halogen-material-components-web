module HalogenMWC.TextArea where

import Halogen (AttrName(..), ElemName(..), PropName(..))
import Material.Classes.Textfield (mdc_floating_label, mdc_floating_label____float_above, mdc_notched_outline, mdc_notched_outline__leading, mdc_notched_outline__notch, mdc_notched_outline__trailing, mdc_text_field, mdc_text_field____disabled, mdc_text_field____fullwidth, mdc_text_field____no_label, mdc_text_field____outlined, mdc_text_field____textarea, mdc_text_field__input)
import Protolude (class Eq, Maybe(..), map, show, ($), (/=), (<<<), (<>), (==))
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Utils as Utils
import DOM.HTML.Indexed as I

type Config i
  = { label :: Maybe String
    , fullwidth :: Boolean
    , value :: Maybe String
    , placeholder :: Maybe String
    , rows :: Maybe Int
    , cols :: Maybe Int
    , disabled :: Boolean
    , required :: Boolean
    , valid :: Boolean
    , minLength :: Maybe Int
    , maxLength :: Maybe Int
    , additionalAttributes :: Array (IProp I.HTMLtextarea i)
    , onInput :: Maybe (String -> i)
    , onChange :: Maybe (String -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { label: Nothing
  , fullwidth: false
  , value: Nothing
  , placeholder: Nothing
  , rows: Nothing
  , cols: Nothing
  , disabled: false
  , required: false
  , valid: true
  , minLength: Nothing
  , maxLength: Nothing
  , additionalAttributes: []
  , onInput: Nothing
  , onChange: Nothing
  }

data TextAreaType
  = Filled
  | Outlined

derive instance eqTextAreaType :: Eq TextAreaType

filled :: forall w i. Config i -> HH.HTML w i
filled = textArea Filled

outlined :: forall w i. Config i -> HH.HTML w i
outlined = textArea Outlined

textArea :: forall w i. TextAreaType -> Config i -> HH.HTML w i
textArea textAreaType config =
  HH.element (ElemName "mdc-text-field")
    ( Array.catMaybes
        [ Just $ HP.classes
            $ Array.concat
                [ [ mdc_text_field, mdc_text_field____textarea ]
                , if config.label == Nothing then [ mdc_text_field____no_label ] else []
                , if textAreaType == Outlined then [ mdc_text_field____outlined ] else []
                , if config.fullwidth then [ mdc_text_field____fullwidth ] else []
                , if config.disabled then [ mdc_text_field____disabled ] else []
                ]
        , map HP.value config.value
        , Just $ HP.disabled config.disabled
        , Just $ HP.required config.required
        , Just $ HP.prop (PropName "valid") config.valid
        , map (HP.attr (AttrName "minLength") <<< show) config.minLength
        , map (HP.attr (AttrName "maxLength") <<< show) config.maxLength
        ]
        <> config.additionalAttributes
    )
    [ inputElt config
    , notchedOutlineElt config
    ]

inputElt :: forall w i. Config i -> HH.HTML w i
inputElt config =
  HH.textarea
    ( Array.catMaybes
        [ Just $ HP.class_ mdc_text_field__input
        , map (HP.attr (AttrName "aria-label")) config.label
        , map HP.rows config.rows
        , map HP.cols config.cols
        , map HP.placeholder config.placeholder
        , map HE.onValueInput config.onInput
        , map HE.onValueChange config.onChange
        , map (HP.attr (AttrName "minLength") <<< show) config.minLength
        , map (HP.attr (AttrName "maxLength") <<< show) config.maxLength
        ]
    )

labelElt :: forall w i. Config i -> HH.HTML w i
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

notchedOutlineElt :: forall w i. Config i -> HH.HTML w i
notchedOutlineElt config =
  HH.div
    [ HP.class_ mdc_notched_outline
    ]
    [ notchedOutlineLeadingElt
    , notchedOutlineNotchElt config
    , notchedOutlineTrailingElt
    ]

notchedOutlineLeadingElt :: forall w i. HH.HTML w i
notchedOutlineLeadingElt = HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElt :: forall w i. HH.HTML w i
notchedOutlineTrailingElt = HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineNotchElt :: forall w i. Config i -> HH.HTML w i
notchedOutlineNotchElt config = HH.div [ HP.class_ mdc_notched_outline__notch ] [ labelElt config ]
