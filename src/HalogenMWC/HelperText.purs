module HalogenMWC.HelperText
    ( Config, config
    , setPersistent
    , setAttributes
    , helperText
    , helperLine, characterCounter
    ) where

{-| Helper text gives context about a field’s input, such as how the input will
be used. It should be visible either persistently or only on focus.


# Table of Contents

  - [Resources](#resources)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Basic Usage](#basic-usage)
  - [Helper Text](#helper-text)
  - [Persistent Helper Text](#persisten-helper-text)
  - [Helper Text with Character Counter](#helper-text-with-character-counter)


# Resources

  - [Demo: Text fields](https://aforemny.github.io/material-components-web-elm/#text-field)
  - [Material Design Guidelines: Text Fields Layout](https://material.io/go/design-text-fields#text-fields-layout)
  - [MDC Web: Text Field Helper Text](https://github.com/material-components/material-components-web/tree/master/packages/mdc-textfield/helper-text)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-textfield/helper-text#sass-mixins)


# Basic Usage

    import Material.HelperText as HelperText
    import Material.TextField as TextField

    main =
        Html.div []
            [ TextField.filled
                (TextField.config
                    |> TextField.setLabel (Just "Your name")
                )
            , HelperText.helperText HelperText.config
                "Please fill this"
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setPersistent
@docs setAttributes


# Helper Text

The helper line is expected to be a direct sibling of the text field it belongs
to and the helper text is expected to be a direct child of the helper text
line.

@docs helperText


# Persistent Helper Text

A text field's helper text may show unconditionally by setting its
`setPersistent` configuration option to `True`. By default a text field's
helper text only shows when the text field has focus.


# Helper Text with Character Counter

To have a text field or text area display a character counter, set its
`setMaxLength` configuration option, and also add a `characterCounter` as a
child of `helperLine`.

    [ TextField.filled
        (TextField.config |> TextField.setMaxLength (Just 18))
    , HelperText.helperLine [] [ HelperText.characterCounter [] ]
    ]

@docs helperLine, characterCounter

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA



{-| Configuration of a helper text
-}
type Config r i
    = Config
        { persistent :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default configuration of a helper text
-}
config :: Config r i
config =
    Config
        { persistent = False
        , additionalAttributes = []
        }


{-| Specify whether a helper text should be persistent

Persistent helper texts always display regardless of whether the input has
focus or not.

-}
setPersistent :: Boolean -> Config r i -> Config r i
setPersistent persistent (Config config_) =
    Config { config_ | persistent = persistent }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Helper text view function

The helper text is expected to be a direct child of the helper line.

-}
helperText :: Config r i -> String -> Html r i
helperText ((Config { additionalAttributes }) as config_) string =
    Html.div
        (Array.filterMap identity
            [ helperTextCs
            , persistentCs config_
            , ariaHiddenAttr
            ]
            ++ additionalAttributes
        )
        [ text string ]


{-| Helper text line view function

The helper line is expected to be the wrapping element of the helper text. It
is expected to be a direct sibling of the text field that it belongs to.

-}
helperLine :: Array (IProp r i) -> Array (Html r i) -> Html r i
helperLine additionalAttributes nodes =
    Html.div (helperLineCs :: additionalAttributes) nodes


helperTextCs :: Maybe (Html.Attribute r i)
helperTextCs =
    Just (class "mdc-text-field-helper-text")


helperLineCs :: Html.Attribute r i
helperLineCs =
    class "mdc-text-field-helper-line"


persistentCs :: Config r i -> Maybe (Html.Attribute r i)
persistentCs (Config config_) =
    if config_.persistent then
        Just (class "mdc-text-field-helper-text--persistent")

    else
        Nothing


ariaHiddenAttr :: Maybe (Html.Attribute r i)
ariaHiddenAttr =
    Just (Html.Attributes.attribute "aria-hidden" "true")


{-| Character counter view function
-}
characterCounter :: Array (IProp r i) -> Html r i
characterCounter additionalAttributes =
    Html.div (characterCounterCs :: additionalAttributes) []


characterCounterCs :: Html.Attribute r i
characterCounterCs =
    class "mdc-text-field-character-counter"
