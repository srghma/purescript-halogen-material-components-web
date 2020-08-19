module HalogenMWC.TextField
    ( Config, config

    , filled
    , outlined
    , Icon, icon
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.Icon as Icon

type Config r i
    =
        { label :: Maybe String
        , fullwidth :: Boolean
        , value :: Maybe String
        , placeholder :: Maybe String
        , disabled :: Boolean
        , required :: Boolean
        , valid :: Boolean
        , minLength :: Maybe Int
        , maxLength :: Maybe Int
        , pattern :: Maybe String
        , type_ :: Maybe String
        , min :: Maybe Int
        , max :: Maybe Int
        , step :: Maybe Int
        , leadingIcon :: Maybe (Icon r i)
        , trailingIcon :: Maybe (Icon r i)
        , additionalAttributes :: Array (IProp r i)
        , onInput :: Maybe (String -> r i)
        , onChange :: Maybe (String -> r i)
        }

{-| Text field trailing or leading icon -
-}
data Icon r i
    = Icon (Html r i)

defaultConfig :: Config r i
defaultConfig =
        { label: Nothing
        , fullwidth: False
        , value: Nothing
        , placeholder: Nothing
        , disabled: False
        , required: False
        , valid: True
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

filled :: Config r i -> Html r i
filled config_ =
    textField False config_

outlined :: Config r i -> Html r i
outlined config_ =
    textField True config_

textField :: Boolean -> Config r i -> Html r i
textField outlined_ (config_@{ additionalAttributes, fullwidth }) =
    HH.node "mdc-text-field"
        (Array.filterMap identity
            [ rootCs
            , noLabelCs config_
            , outlinedCs outlined_
            , fullwidthCs config_
            , disabledCs config_
            , withLeadingIconCs config_
            , withTrailingIconCs config_
            , valueProp config_
            , disabledProp config_
            , requiredProp config_
            , validProp config_
            , patternProp config_
            , minLengthProp config_
            , maxLengthProp config_
            , minProp config_
            , maxProp config_
            , stepProp config_
            ]
            <> additionalAttributes
        )
        (Array.concat
            [ leadingIconElt config_
            , if fullwidth then
                if outlined_ then
                    [ inputElt config_
                    , notchedOutlineElt config_
                    ]

                else
                    [ inputElt config_
                    , lineRippleElt
                    ]

              else if outlined_ then
                [ inputElt config_
                , notchedOutlineElt config_
                ]

              else
                [ inputElt config_
                , labelElt config_
                , lineRippleElt
                ]
            , trailingIconElt config_
            ]
        )

icon :: Array (IProp r i) -> String -> Icon r i
icon additionalAttributes iconName =
    Icon (Icon.icon ([HP.class_ mdc_text_field__icon] <> additionalAttributes) iconName)

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_text_field)

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

withLeadingIconCs :: Config r i -> Maybe (HH.Attribute r i)
withLeadingIconCs { leadingIcon } =
    if leadingIcon /= Nothing then
        Just (HP.class_ "mdc-text-field--with-leading-icon")

    else
        Nothing

withTrailingIconCs :: Config r i -> Maybe (HH.Attribute r i)
withTrailingIconCs { trailingIcon } =
    if trailingIcon /= Nothing then
        Just (HP.class_ "mdc-text-field--with-trailing-icon")

    else
        Nothing

requiredProp :: Config r i -> Maybe (HH.Attribute r i)
requiredProp { required } =
    Just (HH.Attributes.property "required" (Encode.bool required))

validProp :: Config r i -> Maybe (HH.Attribute r i)
validProp { valid } =
    Just (HH.Attributes.property "valid" (Encode.bool valid))

minLengthProp :: Config r i -> Maybe (HH.Attribute r i)
minLengthProp { minLength } =
    Just
        (HH.Attributes.property "minLength"
            (Encode.int (Maybe.withDefault -1 minLength))
        )

maxLengthProp :: Config r i -> Maybe (HH.Attribute r i)
maxLengthProp { maxLength } =
    Just
        (HH.Attributes.property "maxLength"
            (Encode.int (Maybe.withDefault -1 maxLength))
        )

minLengthAttr :: Config r i -> Maybe (HH.Attribute r i)
minLengthAttr { minLength } =
    map (HH.Attributes.attribute "minLength" << String.fromInt) minLength

maxLengthAttr :: Config r i -> Maybe (HH.Attribute r i)
maxLengthAttr { maxLength } =
    map (HH.Attributes.attribute "maxLength" << String.fromInt) maxLength

minProp :: Config r i -> Maybe (HH.Attribute r i)
minProp { min } =
    Just
        (HH.Attributes.property "min"
            (Encode.string (Maybe.withDefault "" (map String.fromInt min)))
        )

maxProp :: Config r i -> Maybe (HH.Attribute r i)
maxProp { max } =
    Just
        (HH.Attributes.property "max"
            (Encode.string (Maybe.withDefault "" (map String.fromInt max)))
        )

stepProp :: Config r i -> Maybe (HH.Attribute r i)
stepProp { step } =
    Just
        (HH.Attributes.property "step"
            (Encode.string (Maybe.withDefault "" (map String.fromInt step)))
        )

valueProp :: Config r i -> Maybe (HH.Attribute r i)
valueProp { value } =
    map (HH.Attributes.property "value" << Encode.string) value

placeholderAttr :: Config r i -> Maybe (HH.Attribute r i)
placeholderAttr { placeholder } =
    map HH.Attributes.placeholder placeholder

leadingIconElt :: Config r i -> Array (Html r i)
leadingIconElt { leadingIcon } =
    case leadingIcon of
        Nothing ->
            []

        Just (Icon html) ->
            [ html ]

trailingIconElt :: Config r i -> Array (Html r i)
trailingIconElt { trailingIcon } =
    case trailingIcon of
        Nothing ->
            []

        Just (Icon html) ->
            [ html ]

inputHandler :: Config r i -> Maybe (HH.Attribute r i)
inputHandler { onInput } =
    map HH.Events.onInput onInput

changeHandler :: Config r i -> Maybe (HH.Attribute r i)
changeHandler { onChange } =
    map (\f -> HH.Events.on "change" (Decode.map f HH.Events.targetValue))
        onChange

inputElt :: Config r i -> Html r i
inputElt config_ =
    HH.input
        (Array.filterMap identity
            [ inputCs
            , typeAttr config_
            , ariaLabelAttr config_
            , placeholderAttr config_
            , inputHandler config_
            , changeHandler config_
            , minLengthAttr config_
            , maxLengthAttr config_
            ]
        )
        []

inputCs :: Maybe (HH.Attribute r i)
inputCs =
    Just (HP.class_ mdc_text_field__input)

patternProp :: Config r i -> Maybe (HH.Attribute r i)
patternProp { pattern } =
    Just
        (HH.Attributes.property "pattern"
            (Maybe.withDefault Encode.null (map Encode.string pattern))
        )

typeAttr :: Config r i -> Maybe (HH.Attribute r i)
typeAttr { type_ } =
    map HH.Attributes.type_ type_

ariaLabelAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaLabelAttr { fullwidth, placeholder, label } =
    if fullwidth then
        map (HH.Attributes.attribute "aria-label") label

    else
        Nothing

disabledProp :: Config r i -> Maybe (HH.Attribute r i)
disabledProp { disabled } =
    Just (HH.Attributes.property "disabled" (Encode.bool disabled))

labelElt :: Config r i -> Html r i
labelElt { label, value } =
    let
        floatingLabelCs =
            "mdc-floating-label"

        floatingLabelFloatAboveCs =
            "mdc-floating-label--float-above"
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

        Nothing ->
            text ""

noLabelCs :: Config r i -> Maybe (HH.Attribute r i)
noLabelCs { label } =
    if label == Nothing then
        Just (HP.class_ "mdc-text-field--no-label")

    else
        Nothing

lineRippleElt :: Html r i
lineRippleElt =
    HH.div [ HP.class_ mdc_line_ripple ] []

notchedOutlineElt :: Config r i -> Html r i
notchedOutlineElt config_ =
    HH.div [ HP.class_ mdc_notched_outline ]
        [ notchedOutlineLeadingElt
        , notchedOutlineNotchElt config_
        , notchedOutlineTrailingElt
        ]

notchedOutlineLeadingElt :: Html r i
notchedOutlineLeadingElt =
    HH.div [ HP.class_ mdc_notched_outline__leading ] []

notchedOutlineTrailingElt :: Html r i
notchedOutlineTrailingElt =
    HH.div [ HP.class_ mdc_notched_outline__trailing ] []

notchedOutlineNotchElt :: Config r i -> Html r i
notchedOutlineNotchElt config_ =
    HH.div [ HP.class_ mdc_notched_outline__notch ] [ labelElt config_ ]
