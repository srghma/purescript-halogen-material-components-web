module HalogenMWC.FormField
    ( Config, config
    , setOnClick
    , setLabel, setAlignEnd
    , setFor
    , setAttributes
    , formField
    ) where

{-| FormField aligns a form field (for example, a checkbox) with
its label and makes it RTL-aware. It also activates a ripple effect upon
interacting with the label.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Form Field](#form-field)
  - [Label Position](#label-position)
  - [Focus a Form Field](#focus-a-form-field)


# Resources

  - [Demo: Checkbox](https://aforemny.github.io/material-components-web-elm/#checkbox)
  - [MDC Web: Form Field](https://github.com/material-components/material-components-web/tree/master/packages/mdc-form-field)


# Basic Usage

    import Material.Checkbox as Checkbox
    import Material.FormField as FormField

    main =
        FormField.formField
            (FormField.config
                |> FormField.setLabel (Just "My checkbox")
            )
            [ Checkbox.checkbox Checkbox.config ]


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClick
@docs setLabel, setAlignEnd
@docs setFor
@docs setAttributes


# Form Field

@docs formField


# Label Position

If you want to position the label after the form field's control, set its
`setAlignEnd` configuration option to `True`.

    FormField.formField
        (FormField.config |> FormField.setAlignEnd True)
        [ Checkbox.checkbox Checkbox.config ]


# Focus a Form Field

You may programatically focus a formfield by assigning an id attribute to it
and use `Browser.Dom.focus`.

    FormField.formField
        (FormField.config
            |> FormField.setAttributes [ Html.Attributes.id "my-form-field" ]
        )
        []

-}

import Html (Html, text)
import Html.Attributes (class)
import Html.Events


{-| Configuration of a form field
-}
type Config r i
    = Config
        { label :: Maybe String
        , for :: Maybe String
        , alignEnd :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }


{-| Specify a form field's label
-}
setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }


{-| Specify a form field label's HTML5 for attribute
-}
setFor :: Maybe String -> Config r i -> Config r i
setFor for (Config config_) =
    Config { config_ | for = for }


{-| Specify whether the form field's label is positioned after its control

This is usefile for, say, checkboxes.

-}
setAlignEnd :: Boolean -> Config r i -> Config r i
setAlignEnd alignEnd (Config config_) =
    Config { config_ | alignEnd = alignEnd }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user clicks on the label
-}
setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }


{-| Default configuration of a form field
-}
config :: Config r i
config =
    Config
        { label = Nothing
        , for = Nothing
        , alignEnd = False
        , additionalAttributes = []
        , onClick = Nothing
        }


{-| Form field view function
-}
formField :: Config r i -> Array (Html r i) -> Html r i
formField ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-form-field"
        (Array.filterMap identity
            [ rootCs
            , alignEndCs config_
            ]
            ++ additionalAttributes
        )
        (nodes ++ [ labelElt config_ ])


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-form-field")


alignEndCs :: Config r i -> Maybe (Html.Attribute r i)
alignEndCs (Config { alignEnd }) =
    if alignEnd then
        Just (class "mdc-form-field--align-end")

    else
        Nothing


forAttr :: Config r i -> Maybe (Html.Attribute r i)
forAttr (Config { for }) =
    Maybe.map Html.Attributes.for for


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick


labelElt :: Config r i -> Html r i
labelElt ((Config { label }) as config_) =
    Html.label
        (Array.filterMap identity
            [ forAttr config_
            , clickHandler config_
            ]
        )
        [ text (Maybe.withDefault "" label) ]
