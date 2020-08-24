module HalogenMWC.TextField where

import Material.Classes.LineRipple
import Material.Classes.Textfield hiding (mdc_line_ripple)
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Icon as Icon
import Web.Event.Event (Event)

type Config r i =
  { label                :: Maybe String
  , fullwidth            :: Boolean
  , value                :: Maybe String
  , placeholder          :: Maybe String
  , disabled             :: Boolean
  , required             :: Boolean
  , valid                :: Boolean
  , minLength            :: Maybe Int
  , maxLength            :: Maybe Int
  , pattern              :: Maybe String
  , type_                :: Maybe String
  , min                  :: Maybe Int
  , max                  :: Maybe Int
  , step                 :: Maybe Int
  , leadingIcon          :: Maybe (Icon r i)
  , trailingIcon         :: Maybe (Icon r i)
  , additionalAttributes :: Array (IProp r i)
  , onInput              :: Maybe (Event -> i)
  , onChange             :: Maybe (Event -> i)
  }

newtype Icon r i = Icon (HH.HTML w i)

defaultConfig :: Config r i
defaultConfig =
  { label:                Nothing
  , fullwidth:            false
  , value:                Nothing
  , placeholder:          Nothing
  , disabled:             false
  , required:             false
  , valid:                true
  , minLength:            Nothing
  , maxLength:            Nothing
  , pattern:              Nothing
  , type_:                Nothing
  , min:                  Nothing
  , max:                  Nothing
  , step:                 Nothing
  , leadingIcon:          Nothing
  , trailingIcon:         Nothing
  , additionalAttributes: []
  , onInput:              Nothing
  , onChange:             Nothing
  }

filled :: Config r i -> HH.HTML w i
filled config = textField false config

outlined :: Config r i -> HH.HTML w i
outlined config = textField true config

textField :: Boolean -> Config r i -> HH.HTML w i
textField outlined_ config =
  HH.element "mdc-text-field"
    ( Array.catMaybes
        [ rootCs
        , noLabelCs config
        , outlinedCs outlined_
        , fullwidthCs config
        , disabledCs config
        , withLeadingIconCs config
        , withTrailingIconCs config
        , valueProp config
        , disabledProp config
        , requiredProp config
        , validProp config
        , patternProp config
        , minLengthProp config
        , maxLengthProp config
        , minProp config
        , maxProp config
        , stepProp config
        ]
        <> config.additionalAttributes
    )
    ( Array.concat
        [ leadingIconElt config
        , if config.fullwidth then
            if outlined_ then
              [ inputElt config
              , notchedOutlineElt config
              ]
            else
              [ inputElt config
              , lineRippleElt
              ]
          else
            if outlined_ then
              [ inputElt config
              , notchedOutlineElt config
              ]
            else
              [ inputElt config
              , labelElt config
              , lineRippleElt
              ]
        , trailingIconElt config
        ]
    )

icon :: Array (IProp r i) -> String -> Icon r i
icon additionalAttributes iconName = Icon (Icon.icon ([ HP.class_ mdc_text_field__icon ] <> additionalAttributes) iconName)

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_text_field)

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

withLeadingIconCs :: Config r i -> Maybe (IProp r i)
withLeadingIconCs { leadingIcon } =
  if leadingIcon /= Nothing then
    Just (HP.class_ mdc_text_field____with_leading_icon)
  else
    Nothing

withTrailingIconCs :: Config r i -> Maybe (IProp r i)
withTrailingIconCs { trailingIcon } =
  if trailingIcon /= Nothing then
    Just (HP.class_ mdc_text_field____with_trailing_icon)
  else
    Nothing

requiredProp :: Config r i -> Maybe (IProp r i)
requiredProp { required } = Just (HP.prop "required" required)

validProp :: Config r i -> Maybe (IProp r i)
validProp { valid } = Just (HP.prop "valid" valid)

minLengthProp :: Config r i -> Maybe (IProp r i)
minLengthProp { minLength } =
  Just
    ( HP.prop "minLength"
        (Encode.int (Maybe.fromMaybe - 1 minLength))
    )

maxLengthProp :: Config r i -> Maybe (IProp r i)
maxLengthProp { maxLength } =
  Just
    ( HP.prop "maxLength"
        (Encode.int (Maybe.fromMaybe - 1 maxLength))
    )

minLengthAttr :: Config r i -> Maybe (IProp r i)
minLengthAttr { minLength } = map (HP.attr "minLength" <<< String.fromInt) minLength

maxLengthAttr :: Config r i -> Maybe (IProp r i)
maxLengthAttr { maxLength } = map (HP.attr "maxLength" <<< String.fromInt) maxLength

minProp :: Config r i -> Maybe (IProp r i)
minProp { min } =
  Just
    ( HP.prop "min"
        (Encode.string (Maybe.fromMaybe "" (map String.fromInt min)))
    )

maxProp :: Config r i -> Maybe (IProp r i)
maxProp { max } =
  Just
    ( HP.prop "max"
        (Encode.string (Maybe.fromMaybe "" (map String.fromInt max)))
    )

stepProp :: Config r i -> Maybe (IProp r i)
stepProp { step } =
  Just
    ( HP.prop "step"
        (Encode.string (Maybe.fromMaybe "" (map String.fromInt step)))
    )

valueProp :: Config r i -> Maybe (IProp r i)
valueProp { value } = map (HP.prop "value" <<< Encode.string) value

placeholderAttr :: Config r i -> Maybe (IProp r i)
placeholderAttr { placeholder } = map HP.placeholder placeholder

leadingIconElt :: Config r i -> Array (HH.HTML w i)
leadingIconElt { leadingIcon } = case leadingIcon of
  Nothing -> []
  Just (Icon html) -> [ html ]

trailingIconElt :: Config r i -> Array (HH.HTML w i)
trailingIconElt { trailingIcon } = case trailingIcon of
  Nothing -> []
  Just (Icon html) -> [ html ]

inputHandler :: Config r i -> Maybe (IProp r i)
inputHandler { onInput } = map HH.Events.onInput onInput

changeHandler :: Config r i -> Maybe (IProp r i)
changeHandler { onChange } =
  map (\f -> HH.Events.on "change" (Decode.map f HH.Events.targetValue))
    onChange

inputElt :: Config r i -> HH.HTML w i
inputElt config =
  HH.input
    ( Array.catMaybes
        [ inputCs
        , typeAttr config
        , ariaLabelAttr config
        , placeholderAttr config
        , inputHandler config
        , changeHandler config
        , minLengthAttr config
        , maxLengthAttr config
        ]
    )
    []

inputCs :: Maybe (IProp r i)
inputCs = Just (HP.class_ mdc_text_field__input)

patternProp :: Config r i -> Maybe (IProp r i)
patternProp { pattern } =
  Just
    ( HP.prop "pattern"
        (Maybe.fromMaybe Encode.null (map Encode.string pattern))
    )

typeAttr :: Config r i -> Maybe (IProp r i)
typeAttr { type_ } = map HP.type_ type_

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
    case label of
      Just str ->
        HH.div
          [ if Maybe.fromMaybe "" value /= "" then
              HP.classes [ mdc_floating_label, mdc_floating_label____float_above ]
            else
              HP.class_ mdc_floating_label
          , HP.prop "foucClassNames"
              (Encode.list Encode.string [ mdc_floating_label____float_above ])
          ]
          [ HH.text str ]
      Nothing -> text ""

noLabelCs :: Config r i -> Maybe (IProp r i)
noLabelCs { label } =
  if label == Nothing then
    Just (HP.class_ mdc_text_field____no_label)
  else
    Nothing

lineRippleElt :: HH.HTML w i
lineRippleElt = HH.div [ HP.class_ mdc_line_ripple ] []

notchedOutlineElt :: Config r i -> HH.HTML w i
notchedOutlineElt config =
  HH.div [ HP.class_ mdc_notched_outline ]
    [ notchedOutlineLeadingElt
    , notchedOutlineNotchElt config
    , notchedOutlineTrailingElt
    ]

notchedOutlineLeadingElt :: HH.HTML w i
notchedOutlineLeadingElt = HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElt :: HH.HTML w i
notchedOutlineTrailingElt = HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineNotchElt :: Config r i -> HH.HTML w i
notchedOutlineNotchElt config = HH.div [ HP.class_ mdc_notched_outline__notch ] [ labelElt config ]
