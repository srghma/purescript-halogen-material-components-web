module HalogenMWC.TextField
    ( Config, config
    , setOnInput
    , setOnChange
    , setLabel
    , setFullwidth
    , setValue
    , setPlaceholder
    , setDisabled
    , setRequired
    , setValid
    , setMinLength
    , setMaxLength
    , setPattern
    , setType
    , setMin
    , setMax
    , setStep
    , setLeadingIcon
    , setTrailingIcon
    , setAttributes
    , filled
    , outlined
    , Icon, icon
    ) where

{-| Text fields allow users to input, edit, and select text.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Filled Text Field](#filled-text-field)
  - [Outlined Text Field](#outlined-text-field)
  - [Full Width Text Field](#full-width-text-field)
  - [Multiline Text Field](#multiline-text-field)
  - [Disabled Text Field](#disabled-text-field)
  - [Password Text Field](#password-text-field)
  - [Required Text Field](#disabled-text-field)
  - [Valid Text Field](#valid-text-field)
  - [Text Field with Leading Icon](#text-field-with-leading-icon)
  - [Text Field with Trailing Icon](#text-field-with-trailing-icon)
  - [Text Field with Character Counter](#text-field-with-character-counter)
  - [Focus a Text Field](#focus-a-text-field)


# Resources

  - [Demo: Text Fields](https://aforemny.github.io/material-components-web-elm/#text-field)
  - [Material Design Guidelines: Menus](https://material.io/go/design-menus)
  - [MDC Web: Menu](https://github.com/material-components/material-components-web/tree/master/packages/mdc-menu)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-menu#sass-mixins)


# Basic Usage

    import Material.TextField as TextField

    data Msg
        = ValueChanged String

    main =
        TextField.filled
            (TextField.config
                |> TextField.setLabel (Just "My text field")
                |> TextField.setValue (Just "hello world")
                |> TextField.setOnInput ValueChanged
            )


# Configuration

@docs Config, config


## Configuration Options

@docs setOnInput
@docs setOnChange
@docs setLabel
@docs setFullwidth
@docs setValue
@docs setPlaceholder
@docs setDisabled
@docs setRequired
@docs setValid
@docs setMinLength
@docs setMaxLength
@docs setPattern
@docs setType
@docs setMin
@docs setMax
@docs setStep
@docs setLeadingIcon
@docs setTrailingIcon
@docs setAttributes


# Filled Text Field


# Filled Text Field

    TextField.filled TextField.config

@docs filled


# Outlined Text Field

Text fields may have a visible outlined around them by using their
`outlined` variant.

    TextField.outlined TextField.config

Note that `setFullwidth` does not have any effect on an outlined text field.

@docs outlined


# Full Width Text Field

To make a text field span all of its available width, set its `setFullwidth`
configuration option to `True`.

    TextField.filled
        (TextField.config |> TextField.setFullwidth True)

Full width text fields do not support a label and will ignore the `setLabel`
configuration option. You may use `setPlaceholder` or provide an extraneous
label for a full width text field.

Outlined text fields do not support `setFullwidth` and wll ignore this
configuration option.


# Disabled Text Field

To disable a text field, set its `setDisabled` configuration option to `True`.

    TextField.filled
        (TextField.config |> TextField.setDisabled True)


# Password Text Field

To mark a text field as an input for entering a passwort, use its `setType`
configuration option to specify `"password"`.

    TextField.filled
        (TextField.config |> TextField.setType (Just "password"))

Other input types besides `"password"` may or may not be supported.


# Required Text Field

To mark a text field as required, set its `setRequired` configuration option to
`True`.

    TextField.filled
        (TextField.config |> TextField.setRequired True)


# Valid Text Field

To mark a text field as valid, set its `setValid` configuration option to
`True`.

    TextField.filled
        (TextField.config |> TextField.setValid True)


# Text Field with Leading Icon

To have a text field display a leading icon, use its `setLeadingIcon`
configuration option to specify a value of `Icon`.

    TextField.filled
        (TextField.config
            |> TextField.setLeadingIcon
                (Just (TextField.icon [] "wifi"))
        )

@docs Icon, icon


# Text Field with Trailing Icon

To have a text field display a trailing icon, use its `setTrailingIcon`
configuration option to specify a value of `Icon`.

    TextField.filled
        (TextField.config
            |> TextField.setTrailingIcon
                (Just (TextField.icon [] "clear"))
        )


# Text Field with Character Counter

To have a text field display a character counter, specify its `setMaxLength`
configuration option, and also add a `HelperText.characterCounter` as a child
of `HelperText.helperLine`.

    [ TextField.filled
        (TextField.config |> TextField.setMaxLength (Just 18))
    , HelperText.helperLine [] [ HelperText.characterCounter [] ]
    ]


# Focus a Text Field

You may programatically focus a text field by assigning an id attribute to it
and use `Browser.Dom.focus`.

    TextField.filled
        (TextField.config
            |> TextField.setAttributes
                [ Html.Attributes.id "my-text-field" ]
        )

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Material.Icon as Icon


{-| Configuration of a text field
-}
type Config r i
    = Config
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


{-| Default configuration of a text field
-}
config :: Config r i
config =
    Config
        { label = Nothing
        , fullwidth = False
        , value = Nothing
        , placeholder = Nothing
        , disabled = False
        , required = False
        , valid = True
        , minLength = Nothing
        , maxLength = Nothing
        , pattern = Nothing
        , type_ = Nothing
        , min = Nothing
        , max = Nothing
        , step = Nothing
        , leadingIcon = Nothing
        , trailingIcon = Nothing
        , additionalAttributes = []
        , onInput = Nothing
        , onChange = Nothing
        }


{-| Specify a text field's label
-}
setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }


{-| Specify a text field to be fullwidth
-}
setFullwidth :: Boolean -> Config r i -> Config r i
setFullwidth fullwidth (Config config_) =
    Config { config_ | fullwidth = fullwidth }


{-| Specify a text field's value
-}
setValue :: Maybe String -> Config r i -> Config r i
setValue value (Config config_) =
    Config { config_ | value = value }


{-| Specify a text field's placeholder
-}
setPlaceholder :: Maybe String -> Config r i -> Config r i
setPlaceholder placeholder (Config config_) =
    Config { config_ | placeholder = placeholder }


{-| Specify a text field to be disabled

Disabled text fields cannot be interacted with and have no visual interaction
effect.

-}
setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify a text field to be required
-}
setRequired :: Boolean -> Config r i -> Config r i
setRequired required (Config config_) =
    Config { config_ | required = required }


{-| Specify a text field to be valid
-}
setValid :: Boolean -> Config r i -> Config r i
setValid valid (Config config_) =
    Config { config_ | valid = valid }


{-| Specify a text field's minimum length
-}
setMinLength :: Maybe Int -> Config r i -> Config r i
setMinLength minLength (Config config_) =
    Config { config_ | minLength = minLength }


{-| Specify a text field's maximum length
-}
setMaxLength :: Maybe Int -> Config r i -> Config r i
setMaxLength maxLength (Config config_) =
    Config { config_ | maxLength = maxLength }


{-| Specify a text field's pattern
-}
setPattern :: Maybe String -> Config r i -> Config r i
setPattern pattern (Config config_) =
    Config { config_ | pattern = pattern }


{-| Specify a text field's type
-}
setType :: Maybe String -> Config r i -> Config r i
setType type_ (Config config_) =
    Config { config_ | type_ = type_ }


{-| Specify a text field's minimum value
-}
setMin :: Maybe Int -> Config r i -> Config r i
setMin min (Config config_) =
    Config { config_ | min = min }


{-| Specify a text field's maximum value
-}
setMax :: Maybe Int -> Config r i -> Config r i
setMax max (Config config_) =
    Config { config_ | max = max }


{-| Specify a text field's step value
-}
setStep :: Maybe Int -> Config r i -> Config r i
setStep step (Config config_) =
    Config { config_ | step = step }


{-| Specify a text field's leading icon
-}
setLeadingIcon :: Maybe (Icon r i) -> Config r i -> Config r i
setLeadingIcon leadingIcon (Config config_) =
    Config { config_ | leadingIcon = leadingIcon }


{-| Specify a text field's trailing icon
-}
setTrailingIcon :: Maybe (Icon r i) -> Config r i -> Config r i
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user changes the value inside the text field
-}
setOnInput :: (String -> r i) -> Config r i -> Config r i
setOnInput onInput (Config config_) =
    Config { config_ | onInput = Just onInput }


{-| Specify a message when the user confirms a changed value inside the text
field
-}
setOnChange :: (String -> r i) -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }


{-| Filled text field view function
-}
filled :: Config r i -> Html r i
filled config_ =
    textField False config_


{-| Outlined text field view function
-}
outlined :: Config r i -> Html r i
outlined config_ =
    textField True config_


textField :: Boolean -> Config r i -> Html r i
textField outlined_ ((Config { additionalAttributes, fullwidth }) as config_) =
    Html.node "mdc-text-field"
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
            ++ additionalAttributes
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


{-| A text field's icon, either leading or trailing
-}
icon :: Array (IProp r i) -> String -> Icon r i
icon additionalAttributes iconName =
    Icon (Icon.icon (class "mdc-text-field__icon" :: additionalAttributes) iconName)


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-text-field")


outlinedCs :: Boolean -> Maybe (Html.Attribute r i)
outlinedCs outlined_ =
    if outlined_ then
        Just (class "mdc-text-field--outlined")

    else
        Nothing


fullwidthCs :: Config r i -> Maybe (Html.Attribute r i)
fullwidthCs (Config { fullwidth }) =
    if fullwidth then
        Just (class "mdc-text-field--fullwidth")

    else
        Nothing


disabledCs :: Config r i -> Maybe (Html.Attribute r i)
disabledCs (Config { disabled }) =
    if disabled then
        Just (class "mdc-text-field--disabled")

    else
        Nothing


withLeadingIconCs :: Config r i -> Maybe (Html.Attribute r i)
withLeadingIconCs (Config { leadingIcon }) =
    if leadingIcon /= Nothing then
        Just (class "mdc-text-field--with-leading-icon")

    else
        Nothing


withTrailingIconCs :: Config r i -> Maybe (Html.Attribute r i)
withTrailingIconCs (Config { trailingIcon }) =
    if trailingIcon /= Nothing then
        Just (class "mdc-text-field--with-trailing-icon")

    else
        Nothing


requiredProp :: Config r i -> Maybe (Html.Attribute r i)
requiredProp (Config { required }) =
    Just (Html.Attributes.property "required" (Encode.bool required))


validProp :: Config r i -> Maybe (Html.Attribute r i)
validProp (Config { valid }) =
    Just (Html.Attributes.property "valid" (Encode.bool valid))


minLengthProp :: Config r i -> Maybe (Html.Attribute r i)
minLengthProp (Config { minLength }) =
    Just
        (Html.Attributes.property "minLength"
            (Encode.int (Maybe.withDefault -1 minLength))
        )


maxLengthProp :: Config r i -> Maybe (Html.Attribute r i)
maxLengthProp (Config { maxLength }) =
    Just
        (Html.Attributes.property "maxLength"
            (Encode.int (Maybe.withDefault -1 maxLength))
        )


minLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
minLengthAttr (Config { minLength }) =
    Maybe.map (Html.Attributes.attribute "minLength" << String.fromInt) minLength


maxLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
maxLengthAttr (Config { maxLength }) =
    Maybe.map (Html.Attributes.attribute "maxLength" << String.fromInt) maxLength


minProp :: Config r i -> Maybe (Html.Attribute r i)
minProp (Config { min }) =
    Just
        (Html.Attributes.property "min"
            (Encode.string (Maybe.withDefault "" (Maybe.map String.fromInt min)))
        )


maxProp :: Config r i -> Maybe (Html.Attribute r i)
maxProp (Config { max }) =
    Just
        (Html.Attributes.property "max"
            (Encode.string (Maybe.withDefault "" (Maybe.map String.fromInt max)))
        )


stepProp :: Config r i -> Maybe (Html.Attribute r i)
stepProp (Config { step }) =
    Just
        (Html.Attributes.property "step"
            (Encode.string (Maybe.withDefault "" (Maybe.map String.fromInt step)))
        )


valueProp :: Config r i -> Maybe (Html.Attribute r i)
valueProp (Config { value }) =
    Maybe.map (Html.Attributes.property "value" << Encode.string) value


placeholderAttr :: Config r i -> Maybe (Html.Attribute r i)
placeholderAttr (Config { placeholder }) =
    Maybe.map Html.Attributes.placeholder placeholder


leadingIconElt :: Config r i -> Array (Html r i)
leadingIconElt (Config { leadingIcon }) =
    case leadingIcon of
        Nothing ->
            []

        Just (Icon html) ->
            [ html ]


trailingIconElt :: Config r i -> Array (Html r i)
trailingIconElt (Config { trailingIcon }) =
    case trailingIcon of
        Nothing ->
            []

        Just (Icon html) ->
            [ html ]


inputHandler :: Config r i -> Maybe (Html.Attribute r i)
inputHandler (Config { onInput }) =
    Maybe.map Html.Events.onInput onInput


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { onChange }) =
    Maybe.map (\f -> Html.Events.on "change" (Decode.map f Html.Events.targetValue))
        onChange


inputElt :: Config r i -> Html r i
inputElt config_ =
    Html.input
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


inputCs :: Maybe (Html.Attribute r i)
inputCs =
    Just (class "mdc-text-field__input")


patternProp :: Config r i -> Maybe (Html.Attribute r i)
patternProp (Config { pattern }) =
    Just
        (Html.Attributes.property "pattern"
            (Maybe.withDefault Encode.null (Maybe.map Encode.string pattern))
        )


typeAttr :: Config r i -> Maybe (Html.Attribute r i)
typeAttr (Config { type_ }) =
    Maybe.map Html.Attributes.type_ type_


ariaLabelAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaLabelAttr (Config { fullwidth, placeholder, label }) =
    if fullwidth then
        Maybe.map (Html.Attributes.attribute "aria-label") label

    else
        Nothing


disabledProp :: Config r i -> Maybe (Html.Attribute r i)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


labelElt :: Config r i -> Html r i
labelElt (Config { label, value }) =
    let
        floatingLabelCs =
            "mdc-floating-label"

        floatingLabelFloatAboveCs =
            "mdc-floating-label--float-above"
    in
    case label of
        Just str ->
            Html.div
                [ if Maybe.withDefault "" value /= "" then
                    class (floatingLabelCs ++ " " ++ floatingLabelFloatAboveCs)

                  else
                    class floatingLabelCs
                , Html.Attributes.property "foucClassNames"
                    (Encode.list Encode.string [ floatingLabelFloatAboveCs ])
                ]
                [ text str ]

        Nothing ->
            text ""


noLabelCs :: Config r i -> Maybe (Html.Attribute r i)
noLabelCs (Config { label }) =
    if label == Nothing then
        Just (class "mdc-text-field--no-label")

    else
        Nothing


lineRippleElt :: Html r i
lineRippleElt =
    Html.div [ class "mdc-line-ripple" ] []


notchedOutlineElt :: Config r i -> Html r i
notchedOutlineElt config_ =
    Html.div [ class "mdc-notched-outline" ]
        [ notchedOutlineLeadingElt
        , notchedOutlineNotchElt config_
        , notchedOutlineTrailingElt
        ]


notchedOutlineLeadingElt :: Html r i
notchedOutlineLeadingElt =
    Html.div [ class "mdc-notched-outline__leading" ] []


notchedOutlineTrailingElt :: Html r i
notchedOutlineTrailingElt =
    Html.div [ class "mdc-notched-outline__trailing" ] []


notchedOutlineNotchElt :: Config r i -> Html r i
notchedOutlineNotchElt config_ =
    Html.div [ class "mdc-notched-outline__notch" ] [ labelElt config_ ]
