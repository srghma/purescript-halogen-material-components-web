module HalogenMWC.Button
    ( Config, config
    , setOnClick
    , setIcon, setTrailingIcon
    , setDisabled
    , setDense
    , setHref, setTarget
    , setTouch
    , setAttributes
    , text, outlined, raised, unelevated
    ) where

{-| Buttons allow users to take actions and make choices with a single tap.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Button Variants](#button-variants)
  - [Button with Icons](#button-with-icons)
      - [Button with Leading Icon](#button-with-leading-icon)
      - [Button with Trailing Icon](#button-with-trailing-icon)
  - [Disabled Button](#disabled-button)
  - [Dense Button](#disabled-button)
  - [Link Button](#link-button)
  - [Focus a Button](#focus-a-button)
  - [Touch Support](#touch-support)


# Resources

  - [Demo: Buttons](https://aforemny.github.io/material-components-web-elm/#buttons)
  - [Material Design Guidelines: Button](https://material.io/go/design-buttons)
  - [MDC Web: Button](https://github.com/material-components/material-components-web/tree/master/packages/mdc-button)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-button#sass-mixins)


# Basic Usage

    import Material.Button as Button

    data Msg
        = Clicked

    main =
        Button.text
            (Button.config |> Button.setOnClick Clicked)
            "Text"


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClick
@docs setIcon, setTrailingIcon
@docs setDisabled
@docs setDense
@docs setHref, setTarget
@docs setTouch
@docs setAttributes


# Button Variants

Buttons may appear in different variants. Use `text` or `outlined` if you want
a button that is flush with the surface, and use `raised` or `unelevated` for a
button that is contained.

@docs text, outlined, raised, unelevated


# Button with Icons

To add an icon to a button, use its `setIcon` configuration option to specify
the name of a [Material Icon](https://material.io/icons). If you want the icon
to be positioned after the button's label, also set the `setTrailingIcon`
configuration option to `True`.


## Button with Leading Icon

    Button.text
        (Button.config |> Button.setIcon (Just "favorite"))
        "Like"


## Button with Trailing Icon

    Button.text
        (Button.config
            |> Button.setIcon (Just "favorite")
            |> Button.setTrailingIcon True
        )
        "Like"


# Disabled Button

To disable a button, use its `setDisabled` configuration option. Disabled
buttons cannot be interacted with and have no visual interaction effect.

    Button.text
        (Button.config |> Button.setDisabled True)
        "Disabled"


# Dense Button

To make a button's text and container margins slightly smaller, use its `setDense`
configuration option.

    Button.text
        (Button.config |> Button.setDense True)
        "Dense"


# Link Button

To make a button essentially behave like a HTML anchor element, use its
`setHref` configution option. You may use its `setTarget` configuration option
to specify a target.

    Button.text
        (Button.config
            |> Button.setHref (Just "https://elm-lang.org")
        )
        "Visit"

Note that link buttons cannot be disabled.


# Focus a Button

You may programatically focus a button by assigning an id attribute to it and
use `Browser.Dom.focus`.

    Button.text
        (Button.config
            |> Button.setAttributes
                [ Html.Attributes.id "my-button" ]
        )
        "Button"


# Touch Support

Touch support is enabled by default. To disable touch support set a button's
`setTouch` configuration option to `False`.

    Button.text
        (Button.config
            |> Button.setTouch False
        )
        "Click"

-}

import Html (Html)
import Html.Attributes (class)
import Html.Events
import Json.Encode as Encode
import Material.Button.Internal (Config(..))


{-| Configuration of a button
-}
data Config msg =
    Material.Button.Internal.Config msg


{-| Default configuration of a button
-}
config :: Config msg
config =
    Config
        { icon = Nothing
        , trailingIcon = False
        , disabled = False
        , dense = False
        , href = Nothing
        , target = Nothing
        , additionalAttributes = []
        , onClick = Nothing
        , touch = True
        }


{-| Specify whether the button features an icon
-}
setIcon :: Maybe String -> Config msg -> Config msg
setIcon icon (Config config_) =
    Config { config_ | icon = icon }


{-| Specify whether a button's icon is a _trailing icon_.

Trailing icons are displayed after the button's label rather than before.

-}
setTrailingIcon :: Bool -> Config msg -> Config msg
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }


{-| Specify whether the button is disabled

Disabled buttons cannot be interacted with and do not have no visual
interaction effect.

-}
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify whether a button is _dense_

Dense buttons feature smaller than normal padding.

-}
setDense :: Bool -> Config msg -> Config msg
setDense dense (Config config_) =
    Config { config_ | dense = dense }


{-| Specify whether a button is a _link button_.

Link buttons behave like normal HTML5 anchor tags. Note that link buttons
cannot be disabled and ignore that configuration option.

-}
setHref :: Maybe String -> Config msg -> Config msg
setHref href (Config config_) =
    Config { config_ | href = href }


{-| Specify the target for a link button.

Note that this configuration option will be ignored by buttons that do not also
set `setHref`.

-}
setTarget :: Maybe String -> Config msg -> Config msg
setTarget target (Config config_) =
    Config { config_ | target = target }


{-| Specify additional attributes
-}
setAttributes :: List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user clicks a button
-}
setOnClick :: msg -> Config msg -> Config msg
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }


{-| Specify whether touch support is enabled (enabled by default)

Touch support is an accessibility guideline that states that touch targets
should be at least 48 x 48 pixels in size. Use this configuration option to
disable increased touch target size.

**Note:** Buttons with touch support will be wrapped in a HTML div element to
prevent potentially overlapping touch targets on adjacent elements.

-}
setTouch :: Bool -> Config msg -> Config msg
setTouch touch (Config config_) =
    Config { config_ | touch = touch }


data Variant
    = Text
    | Raised
    | Unelevated
    | Outlined


button :: Variant -> Config msg -> String -> Html msg
button variant ((Config { additionalAttributes, touch, href }) as config_) label =
    let
        wrapTouch node =
            if touch then
                Html.div [ class "mdc-touch-target-wrapper" ] [ node ]

            else
                node
    in
    wrapTouch <|
        Html.node "mdc-button"
            (List.filterMap identity [ disabledProp config_ ])
            [ (if href /= Nothing then
                Html.a

               else
                Html.button
              )
                (List.filterMap identity
                    [ rootCs
                    , variantCs variant
                    , denseCs config_
                    , touchCs config_
                    , disabledAttr config_
                    , tabIndexProp config_
                    , hrefAttr config_
                    , targetAttr config_
                    , clickHandler config_
                    ]
                    ++ additionalAttributes
                )
                (List.filterMap identity
                    [ rippleElt
                    , leadingIconElt config_
                    , labelElt label
                    , trailingIconElt config_
                    , touchElt config_
                    ]
                )
            ]


{-| Text button variant (flush without outline)
-}
text :: Config msg -> String -> Html msg
text config_ label =
    button Text config_ label


{-| Outlined button variant (flush with outline)
-}
outlined :: Config msg -> String -> Html msg
outlined config_ label =
    button Outlined config_ label


{-| Raised button variant (contained with elevation)
-}
raised :: Config msg -> String -> Html msg
raised config_ label =
    button Raised config_ label


{-| Unelevated button variant (contained without elevation)
-}
unelevated :: Config msg -> String -> Html msg
unelevated config_ label =
    button Unelevated config_ label


rootCs :: Maybe (Html.Attribute msg)
rootCs =
    Just (class "mdc-button")


disabledProp :: Config msg -> Maybe (Html.Attribute msg)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


disabledAttr :: Config msg -> Maybe (Html.Attribute msg)
disabledAttr (Config { disabled }) =
    Just (Html.Attributes.disabled disabled)


tabIndexProp :: Config msg -> Maybe (Html.Attribute msg)
tabIndexProp (Config { disabled }) =
    if disabled then
        Just (Html.Attributes.property "tabIndex" (Encode.int -1))

    else
        Just (Html.Attributes.property "tabIndex" (Encode.int 0))


hrefAttr :: Config msg -> Maybe (Html.Attribute msg)
hrefAttr (Config { href }) =
    Maybe.map Html.Attributes.href href


targetAttr :: Config msg -> Maybe (Html.Attribute msg)
targetAttr (Config { href, target }) =
    if href /= Nothing then
        Maybe.map Html.Attributes.target target

    else
        Nothing


clickHandler :: Config msg -> Maybe (Html.Attribute msg)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick


variantCs :: Variant -> Maybe (Html.Attribute msg)
variantCs variant =
    case variant of
        Text ->
            Nothing

        Raised ->
            Just (class "mdc-button--raised")

        Unelevated ->
            Just (class "mdc-button--unelevated")

        Outlined ->
            Just (class "mdc-button--outlined")


denseCs :: Config msg -> Maybe (Html.Attribute msg)
denseCs (Config { dense }) =
    if dense then
        Just (class "mdc-button--dense")

    else
        Nothing


touchCs :: Config msg -> Maybe (Html.Attribute msg)
touchCs (Config { touch }) =
    if touch then
        Just (class "mdc-button--touch")

    else
        Nothing


iconElt :: Config msg -> Maybe (Html msg)
iconElt (Config { icon }) =
    Maybe.map
        (\iconName ->
            Html.i
                [ class "mdc-button__icon material-icons"
                , Html.Attributes.attribute "aria-hidden" "true"
                ]
                [ Html.text iconName ]
        )
        icon


rippleElt :: Maybe (Html msg)
rippleElt =
    Just (Html.div [ class "mdc-button__ripple" ] [])


leadingIconElt :: Config msg -> Maybe (Html msg)
leadingIconElt ((Config { trailingIcon }) as config_) =
    if not trailingIcon then
        iconElt config_

    else
        Nothing


trailingIconElt :: Config msg -> Maybe (Html msg)
trailingIconElt ((Config { trailingIcon }) as config_) =
    if trailingIcon then
        iconElt config_

    else
        Nothing


touchElt :: Config msg -> Maybe (Html msg)
touchElt (Config { touch }) =
    if touch then
        Just (Html.div [ class "mdc-button__touch" ] [])

    else
        Nothing


labelElt :: String -> Maybe (Html msg)
labelElt label =
    Just (Html.span [ class "mdc-button__label" ] [ Html.text label ])
