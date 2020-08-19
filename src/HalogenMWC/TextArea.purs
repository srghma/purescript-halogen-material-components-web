module HalogenMWC.TextArea where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

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
  , fullwidth: False
  , value: Nothing
  , placeholder: Nothing
  , rows: Nothing
  , cols: Nothing
  , disabled: False
  , required: False
  , valid: True
  , minLength: Nothing
  , maxLength: Nothing
  , additionalAttributes: []
  , onInput: Nothing
  , onChange: Nothing
  }

filled :: Config r i -> Html r i
filled config_ = textArea False config_

outlined :: Config r i -> Html r i
outlined config_ = textArea True config_

textArea :: Boolean -> Config r i -> Html r i
textArea outlined_ (config_@{ additionalAttributes, fullwidth }) =
  HH.node "mdc-text-field"
    ( Array.filterMap identity
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

rootCs :: Maybe (HH.Attribute r i)
rootCs = Just (HP.class_ "mdc-text-field mdc-text-field--textarea")

outlinedCs :: Boolean -> Maybe (HH.Attribute r i)
outlinedCs outlined_ =
  if outlined_ then
    Just (HP.class_ mdc_text_field____outlined)
  else
    Nothing

fullwidthCs :: Config r i -> Maybe (HH.Attribute r i)
fullwidthCs { fullwidth } =
  if fullwidth then
    Just (HP.class_ mdc_text_field____fullwidth)
  else
    Nothing

disabledCs :: Config r i -> Maybe (HH.Attribute r i)
disabledCs { disabled } =
  if disabled then
    Just (HP.class_ mdc_text_field____disabled)
  else
    Nothing

requiredProp :: Config r i -> Maybe (HH.Attribute r i)
requiredProp { required } = Just (HH.Attributes.property "required" (Encode.bool required))

validProp :: Config r i -> Maybe (HH.Attribute r i)
validProp { valid } = Just (HH.Attributes.property "valid" (Encode.bool valid))

minLengthAttr :: Config r i -> Maybe (HH.Attribute r i)
minLengthAttr { minLength } = map (HH.Attributes.attribute "minLength" << String.fromInt) minLength

maxLengthAttr :: Config r i -> Maybe (HH.Attribute r i)
maxLengthAttr { maxLength } = map (HH.Attributes.attribute "maxLength" << String.fromInt) maxLength

valueProp :: Config r i -> Maybe (HH.Attribute r i)
valueProp { value } = map (HH.Attributes.property "value" << Encode.string) value

placeholderAttr :: Config r i -> Maybe (HH.Attribute r i)
placeholderAttr { placeholder } = map HH.Attributes.placeholder placeholder

inputHandler :: Config r i -> Maybe (HH.Attribute r i)
inputHandler { onInput } = map HH.Events.onInput onInput

changeHandler :: Config r i -> Maybe (HH.Attribute r i)
changeHandler { onChange } =
  map (\f -> HH.Events.on "change" (Decode.map f HH.Events.targetValue))
    onChange

inputElt :: Config r i -> Html r i
inputElt config_ =
  HH.textarea
    ( Array.filterMap identity
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

inputCs :: Maybe (HH.Attribute r i)
inputCs = Just (HP.class_ mdc_text_field__input)

rowsAttr :: Config r i -> Maybe (HH.Attribute r i)
rowsAttr { rows } = map HH.Attributes.rows rows

colsAttr :: Config r i -> Maybe (HH.Attribute r i)
colsAttr { cols } = map HH.Attributes.cols cols

ariaLabelAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaLabelAttr { fullwidth, placeholder, label } =
  if fullwidth then
    map (HH.Attributes.attribute "aria-label") label
  else
    Nothing

disabledProp :: Config r i -> Maybe (HH.Attribute r i)
disabledProp { disabled } = Just (HH.Attributes.property "disabled" (Encode.bool disabled))

labelElt :: Config r i -> Html r i
labelElt { label, value } =
  let
    floatingLabelCs = "mdc-floating-label"

    floatingLabelFloatAboveCs = "mdc-floating-label--float-above"
  in
    case label of
      Just str ->
        HH.div
          [ if Maybe.withDefault "" value /= "" then
              HP.class_ (floatingLabelCs <> " " <> floatingLabelFloatAboveCs)
            else
              HP.class_ floatingLabelCs
          , HH.Attributes.property "foucClassNames"
              (Encode.list Encode.string [ floatingLabelFloatAboveCs ])
          ]
          [ text str ]
      Nothing -> text ""

noLabelCs :: Config r i -> Maybe (HH.Attribute r i)
noLabelCs { label } =
  if label == Nothing then
    Just (HP.class_ "mdc-text-field--no-label")
  else
    Nothing

notchedOutlineElt :: Config r i -> Html r i
notchedOutlineElt config_ =
  HH.div [ HP.class_ mdc_notched_outline ]
    [ notchedOutlineLeadingElt
    , notchedOutlineNotchElt config_
    , notchedOutlineTrailingElt
    ]

notchedOutlineLeadingElt :: Html r i
notchedOutlineLeadingElt = HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElt :: Html r i
notchedOutlineTrailingElt = HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineNotchElt :: Config r i -> Html r i
notchedOutlineNotchElt config_ = HH.div [ HP.class_ mdc_notched_outline__notch ] [ labelElt config_ ]
