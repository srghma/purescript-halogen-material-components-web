module HalogenMWC.TextArea where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe

import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Textfield

type Config r i
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
    , additionalAttributes :: Array (IProp r i)
    , onInput :: Maybe (String -> r i)
    , onChange :: Maybe (String -> r i)
    }

defaultConfig :: Config r i
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

filled :: Config r i -> HH.HTML w i
filled config_ = textArea false config_

outlined :: Config r i -> HH.HTML w i
outlined config_ = textArea true config_

textArea :: Boolean -> Config r i -> HH.HTML w i
textArea outlined_ (config_@{ additionalAttributes, fullwidth }) =
  HH.element "mdc-text-field"
    ( Array.catMaybes
        [ rootCs
        , noLabelCs config_
        , outlinedCs outlined_
        , fullwidthCs config_
        , disabledCs config_
        , valueProp config_
        , disabledProp config_
        , requiredProp config_
        , validProp config_
        , minLengthAttr config_
        , maxLengthAttr config_
        ]
        <> additionalAttributes
    )
    ( Array.concat
        [ if fullwidth then
            [ inputElt config_
            , notchedOutlineElt config_
            ]
          else
            [ inputElt config_
            , notchedOutlineElt config_
            ]
        ]
    )

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.classes [mdc_text_field, mdc_text_field____textarea])

outlinedCs :: Boolean -> Maybe (IProp r i)
outlinedCs outlined_ =
  if outlined_ then
    Just (HP.class_ mdc_text_field____outlined)
  else
    Nothing

fullwidthCs :: Config r i -> Maybe (IProp r i)
fullwidthCs { fullwidth } =
  if fullwidth then
    Just (HP.class_ mdc_text_field____fullwidth)
  else
    Nothing

disabledCs :: Config r i -> Maybe (IProp r i)
disabledCs { disabled } =
  if disabled then
    Just (HP.class_ mdc_text_field____disabled)
  else
    Nothing

requiredProp :: Config r i -> Maybe (IProp r i)
requiredProp { required } = Just (HP.prop "required" required)

validProp :: Config r i -> Maybe (IProp r i)
validProp { valid } = Just (HP.prop "valid" valid)

minLengthAttr :: Config r i -> Maybe (IProp r i)
minLengthAttr { minLength } = map (HP.attr "minLength" << String.fromInt) minLength

maxLengthAttr :: Config r i -> Maybe (IProp r i)
maxLengthAttr { maxLength } = map (HP.attr "maxLength" << String.fromInt) maxLength

valueProp :: Config r i -> Maybe (IProp r i)
valueProp { value } = map (HP.prop "value" << Encode.string) value

placeholderAttr :: Config r i -> Maybe (IProp r i)
placeholderAttr { placeholder } = map HP.placeholder placeholder

inputHandler :: Config r i -> Maybe (IProp r i)
inputHandler { onInput } = map HH.Events.onInput onInput

changeHandler :: Config r i -> Maybe (IProp r i)
changeHandler { onChange } =
  map (\f -> HH.Events.on "change" (Decode.map f HH.Events.targetValue))
    onChange

inputElt :: Config r i -> HH.HTML w i
inputElt config_ =
  HH.textarea
    ( Array.catMaybes
        [ inputCs
        , ariaLabelAttr config_
        , rowsAttr config_
        , colsAttr config_
        , placeholderAttr config_
        , inputHandler config_
        , changeHandler config_
        , minLengthAttr config_
        , maxLengthAttr config_
        ]
    )
    []

inputCs :: Maybe (IProp r i)
inputCs = Just (HP.class_ mdc_text_field__input)

rowsAttr :: Config r i -> Maybe (IProp r i)
rowsAttr { rows } = map HP.rows rows

colsAttr :: Config r i -> Maybe (IProp r i)
colsAttr { cols } = map HP.cols cols

ariaLabelAttr :: Config r i -> Maybe (IProp r i)
ariaLabelAttr { fullwidth, placeholder, label } =
  if fullwidth then
    map (HP.attr "aria-label") label
  else
    Nothing

disabledProp :: Config r i -> Maybe (IProp r i)
disabledProp { disabled } = Just (HP.prop "disabled" disabled)

labelElt :: Config r i -> HH.HTML w i
labelElt { label, value } =
  let
    floatingLabelCs = "mdc-floating-label"

    floatingLabelFloatAboveCs = "mdc-floating-label--float-above"
  in
    case label of
      Just str ->
        HH.div
          [ if Maybe.fromMaybe "" value /= "" then
              HP.class_ (floatingLabelCs <> " " <> floatingLabelFloatAboveCs)
            else
              HP.class_ floatingLabelCs
          , HP.prop "foucClassNames"
              (Encode.list Encode.string [ floatingLabelFloatAboveCs ])
          ]
          [ text str ]
      Nothing -> text ""

noLabelCs :: Config r i -> Maybe (IProp r i)
noLabelCs { label } =
  if label == Nothing then
    Just (HP.class_ mdc_text_field____no_label)
  else
    Nothing

notchedOutlineElt :: Config r i -> HH.HTML w i
notchedOutlineElt config_ =
  HH.div [ HP.class_ mdc_notched_outline ]
    [ notchedOutlineLeadingElt
    , notchedOutlineNotchElt config_
    , notchedOutlineTrailingElt
    ]

notchedOutlineLeadingElt :: HH.HTML w i
notchedOutlineLeadingElt = HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElt :: HH.HTML w i
notchedOutlineTrailingElt = HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineNotchElt :: Config r i -> HH.HTML w i
notchedOutlineNotchElt config_ = HH.div [ HP.class_ mdc_notched_outline__notch ] [ labelElt config_ ]
