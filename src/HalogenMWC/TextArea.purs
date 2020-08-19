module HalogenMWC.TextArea
    ( Config, config
    , setOnInput
    , setOnChange
    , setLabel
    , setFullwidth
    , setValue
    , setPlaceholder
    , setRows
    , setCols
    , setDisabled
    , setRequired
    , setValid
    , setMinLength
    , setMaxLength
    , setAttributes
    , filled
    , outlined
    ) where

{-| Text areas allow users to input, edit, and select multiline text.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Filled Text Area](#filled-text-area)
  - [Outlined Text Area](#outlined-text-area)
  - [Full Width Text Area](#full-width-text-area)
  - [Disabled Text Area](#disabled-text-area)
  - [Required Text Area](#required-text-area)
  - [Valid Text Area](#valid-text-area)
  - [Text Area with Character Counter](#text-area-with-character-counter)
  - [Focus a Text Area](#focus-a-text-area)


# Resources

  - [Demo: Text Areas](https://aforemny.github.io/material-components-web-elm/#text-field)
  - [Material Design Guidelines: Menus](https://material.io/go/design-menus)
  - [MDC Web: Menu](https://github.com/material-components/material-components-web/tree/master/packages/mdc-menu)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-menu#sass-mixins)


# Basic Usage

    import HalogenMWC.TextArea as TextArea

    data Msg
        = ValueChanged String

    main =
        TextArea.filled
            (TextArea.config
                |> TextArea.setLabel (Just "My text area")
                |> TextArea.setValue (Just "hello world")
                |> TextArea.setOnInput ValueChanged
                |> TextArea.setRows (Just 4)
                |> TextArea.setCols (Just 20)
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
@docs setRows
@docs setCols
@docs setDisabled
@docs setRequired
@docs setValid
@docs setMinLength
@docs setMaxLength
@docs setAttributes


# Filled Text Area

    TextArea.filled TextArea.config

@docs filled


# Outlined Text Area

Text areas may have a visible outlined around them by using their `outlined`
variant.

    TextArea.outlined TextArea.config

Note that `setFullwidth` does not have any effect on an outlined text area.

@docs outlined


# Full Width Text Area

To make a text area span all of its available width, set its `setFullwidth`
configuration option to `True`.

    TextArea.filled
        (TextArea.config |> TextArea.setFullwidth True)

Full width text areas do not support labels and will ignore the `setLabel`
configuration option. You may use `setPlaceholder` or provide an extraneous
label for a full width text area.

Outlined text areas do not support `setFullwidth` and will ignore this
configuration option.


# Disabled Text Area

To disable a text area, set its `setDisabled` configuration option to `True`.

    TextArea.filled
        (TextArea.config |> TextArea.setDisabled True)


# Required Text Area

To mark a text area as required, set its `setRequired` configuration option to
`True`.

    TextArea.filled
        (TextArea.config |> TextArea.setRequired True)


# Valid Text Area

To mark a text area as valid, set its `setValid` configuration option to
`True`.

    TextArea.filled (TextArea.config |> TextArea.setValid True)


# Text Area with Character Counter

To have a text area display a character counter, specify its `setMaxLength`
configuration option, and also add a `HelperText.characterCounter` as a child
of `HelperText.helperLine`.

    [ TextArea.filled
        (TextArea.config |> TextArea.setMaxLength (Just 18))
    , HelperText.helperLine [] [ HelperText.characterCounter [] ]
    ]


# Focus a Text Area

You may programatically focus a text area by assigning an id attribute to it
and use `Browser.Dom.focus`.

    TextArea.filled
        (TextArea.config
            |> TextArea.setAttributes
                [ Html.Attributes.id "my-text-area" ]
        )

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA






{-| Configuration of a text area
-}
type Config r i
    = Config
        { label :: Maybe String
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


{-| Default configuration of a text area
-}
config :: Config r i
config =
    Config
        { label = Nothing
        , fullwidth = False
        , value = Nothing
        , placeholder = Nothing
        , rows = Nothing
        , cols = Nothing
        , disabled = False
        , required = False
        , valid = True
        , minLength = Nothing
        , maxLength = Nothing
        , additionalAttributes = []
        , onInput = Nothing
        , onChange = Nothing
        }


{-| Specify a text area's label
-}
setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }


{-| Specify a text area to be fullwidth
-}
setFullwidth :: Boolean -> Config r i -> Config r i
setFullwidth fullwidth (Config config_) =
    Config { config_ | fullwidth = fullwidth }


{-| Specify a text area's value
-}
setValue :: Maybe String -> Config r i -> Config r i
setValue value (Config config_) =
    Config { config_ | value = value }


{-| Specify a text area's placeholder
-}
setPlaceholder :: Maybe String -> Config r i -> Config r i
setPlaceholder placeholder (Config config_) =
    Config { config_ | placeholder = placeholder }


{-| Specify a text area's number of rows
-}
setRows :: Maybe Int -> Config r i -> Config r i
setRows rows (Config config_) =
    Config { config_ | rows = rows }


{-| Specify a text area's number of columns
-}
setCols :: Maybe Int -> Config r i -> Config r i
setCols cols (Config config_) =
    Config { config_ | cols = cols }


{-| Specify a text area to be disabled

Disabled text areas cannot be interacted with and have no visual interaction
effect.

-}
setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify a text area to be required
-}
setRequired :: Boolean -> Config r i -> Config r i
setRequired required (Config config_) =
    Config { config_ | required = required }


{-| Specify a text area to be valid
-}
setValid :: Boolean -> Config r i -> Config r i
setValid valid (Config config_) =
    Config { config_ | valid = valid }


{-| Specify a text area's minimum length
-}
setMinLength :: Maybe Int -> Config r i -> Config r i
setMinLength minLength (Config config_) =
    Config { config_ | minLength = minLength }


{-| Specify a text area's maximum length
-}
setMaxLength :: Maybe Int -> Config r i -> Config r i
setMaxLength maxLength (Config config_) =
    Config { config_ | maxLength = maxLength }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user changes the value inside the text area
-}
setOnInput :: (String -> r i) -> Config r i -> Config r i
setOnInput onInput (Config config_) =
    Config { config_ | onInput = Just onInput }


{-| Specify a message when the user confirms a changed value inside the text
area
-}
setOnChange :: (String -> r i) -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }


{-| Filled text area view function
-}
filled :: Config r i -> Html r i
filled config_ =
    textArea False config_


{-| Outlined text area view function
-}
outlined :: Config r i -> Html r i
outlined config_ =
    textArea True config_


textArea :: Boolean -> Config r i -> Html r i
textArea outlined_ ((Config { additionalAttributes, fullwidth }) as config_) =
    Html.node "mdc-text-field"
        (Array.filterMap identity
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
            ++ additionalAttributes
        )
        (Array.concat
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


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-text-field mdc-text-field--textarea")


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


requiredProp :: Config r i -> Maybe (Html.Attribute r i)
requiredProp (Config { required }) =
    Just (Html.Attributes.property "required" (Encode.bool required))


validProp :: Config r i -> Maybe (Html.Attribute r i)
validProp (Config { valid }) =
    Just (Html.Attributes.property "valid" (Encode.bool valid))


minLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
minLengthAttr (Config { minLength }) =
    Maybe.map (Html.Attributes.attribute "minLength" << String.fromInt) minLength


maxLengthAttr :: Config r i -> Maybe (Html.Attribute r i)
maxLengthAttr (Config { maxLength }) =
    Maybe.map (Html.Attributes.attribute "maxLength" << String.fromInt) maxLength


valueProp :: Config r i -> Maybe (Html.Attribute r i)
valueProp (Config { value }) =
    Maybe.map (Html.Attributes.property "value" << Encode.string) value


placeholderAttr :: Config r i -> Maybe (Html.Attribute r i)
placeholderAttr (Config { placeholder }) =
    Maybe.map Html.Attributes.placeholder placeholder


inputHandler :: Config r i -> Maybe (Html.Attribute r i)
inputHandler (Config { onInput }) =
    Maybe.map Html.Events.onInput onInput


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { onChange }) =
    Maybe.map (\f -> Html.Events.on "change" (Decode.map f Html.Events.targetValue))
        onChange


inputElt :: Config r i -> Html r i
inputElt config_ =
    Html.textarea
        (Array.filterMap identity
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


inputCs :: Maybe (Html.Attribute r i)
inputCs =
    Just (class "mdc-text-field__input")


rowsAttr :: Config r i -> Maybe (Html.Attribute r i)
rowsAttr (Config { rows }) =
    Maybe.map Html.Attributes.rows rows


colsAttr :: Config r i -> Maybe (Html.Attribute r i)
colsAttr (Config { cols }) =
    Maybe.map Html.Attributes.cols cols


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
