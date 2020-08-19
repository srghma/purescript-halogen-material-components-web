module HalogenMWC.Fab.Extended
    ( Config, config
    
    
    
    
    , fab
    ) where

{-| A floating action button represents the primary action in an application.

An extended floating action button primarily contains text to indicate its
action, and optionally contains an icon. If you are looking for a floating
action button that primarily contains an icon, and no text, refer to the
[regular floating action button](Material-Fab).


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Extended Floating Action Button](#extended-floating-action-button)
  - [Extended FAB with Icon](#extended-fab-with-icon)
      - [Extended FAB with Leading Icon](#extended-fab-with-leading-icon)
      - [Extended FAB with Trailing Icon](#extended-fab-with-trailing-icon)
  - [Exited Extended FAB](#exited-extended-fab)
  - [Focus an Extended FAB](#focus-an-extended-fab)


# Resources

  - [Demo: Floating action buttons](https://aforemny.github.io/material-components-web-elm/#fab)
  - [Material Design Guidelines: Floating Action Button](https://material.io/go/design-fab)
  - [MDC Web: Floating Action Button](https://github.com/material-components/material-components-web/tree/master/packages/mdc-fab)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-fab#sass-mixins)


# Basic Usage

Developers are required to manually position the floating action button within
their page layout, for instance by setting a fixed position via CSS.

    
    import HalogenMWC.Fab.Extended as ExtendedFab

    data Msg
        = Clicked

    main =
        ExtendedFab.fab
            (ExtendedFab.config
                |> ExtendedFab.setOnClick FabClicked
                |> ExtendedFab.setAttributes
                    [ style "position" "fixed"
                    , style "bottom" "2rem"
                    , style "right" "2rem"
                    ]
            )
            "Favorites"


## Configuration

@docs Config, config


### Configuration Options

@docs setOnClick
@docs setIcon
@docs setExited
@docs setAttributes


# Extended Floating Action Button

@docs fab


## Extended FAB with Icon

To add an icon to an extended floating action button, use its `setIcon`
configuration option and specify the name of a [Material
Icon](https://material.io/icons). If you want the icon to be positioned after
the button's label, also set its `setTrailingIcon` configuration option to
`True`.


### Extended FAB with Leading Icon

    ExtendedFab.fab
        (ExtendedFab.config
            |> ExtendedFab.setIcon (Just "favorite")
        )
        "Favorites"


### Extended FAB with Trailing Icon

    ExtendedFab.fab
        (ExtendedFab.config
            |> ExtendedFab.setIcon (Just "favorite")
            |> ExtendedFab.setTrailingIcon True
        )
        "Favorites"


## Exited Extended FAB

If you want the extended floating action button to transition off the screen,
set its `setExited` configuration option to `True`.

    ExtendedFab.fab
        (ExtendedFab.config |> ExtendedFab.setExited True)
        "Favorites"


# Focus an Extended FAB

You may programatically focus an extended floating action button by assigning
an id attribute to it and use `Browser.Dom.focus`.

    ExtendedFab.fab
        (ExtendedFab.config
            |> ExtendedFab.setAttributes
                [ Html.Attributes.id "my-fab" ]
        )
        "favorite_border"

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA






type Config r i
    =
        { icon :: Maybe String
        , trailingIcon :: Boolean
        , exited :: Boolean
        , onClick :: Maybe r i
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { icon = Nothing
        , trailingIcon = False
        , exited = False
        , onClick = Nothing
        , additionalAttributes = []
        }



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setTrailingIcon :: Boolean -> Config r i -> Config r i
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }



setExited :: Boolean -> Config r i -> Config r i
setExited exited (Config config_) =
    Config { config_ | exited = exited }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



fab :: Config r i -> String -> Html r i
fab ((Config { additionalAttributes }) as config_) label =
    Html.node "mdc-fab"
        (Array.filterMap identity
            [ rootCs
            , extendedFabCs
            , exitedCs config_
            , clickHandler config_
            , tabIndexProp 0
            ]
            ++ additionalAttributes
        )
        (Array.filterMap identity
            [ rippleElt
            , leadingIconElt config_
            , labelElt label
            , trailingIconElt config_
            ]
        )


tabIndexProp :: Int -> Maybe (Html.Attribute r i)
tabIndexProp tabIndex =
    Just (Html.Attributes.property "tabIndex" (Encode.int tabIndex))


extendedFabCs :: Maybe (Html.Attribute r i)
extendedFabCs =
    Just (class "mdc-fab mdc-fab--extended")


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (Html.div [ class "mdc-fab__ripple" ] [])


leadingIconElt :: Config r i -> Maybe (Html r i)
leadingIconElt (Config { icon, trailingIcon }) =
    case ( icon, trailingIcon ) of
        ( Just iconName, False ) ->
            Just
                (Html.span [ class "material-icons", class "mdc-fab__icon" ]
                    [ text iconName ]
                )

        _ ->
            Nothing


labelElt :: String -> Maybe (Html r i)
labelElt label =
    Just (Html.span [ class "mdc-fab__label" ] [ text label ])


trailingIconElt :: Config r i -> Maybe (Html r i)
trailingIconElt (Config { icon, trailingIcon }) =
    case ( icon, trailingIcon ) of
        ( Just iconName, True ) ->
            Just
                (Html.span [ class "material-icons", class "mdc-fab__icon" ]
                    [ text iconName ]
                )

        _ ->
            Nothing


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-fab")


exitedCs :: Config r i -> Maybe (Html.Attribute r i)
exitedCs (Config { exited }) =
    if exited then
        Just (class "mdc-fab--exited")

    else
        Nothing


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick
