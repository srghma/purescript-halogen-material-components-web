module HalogenMWC.Menu
    ( Config, config
    , setOnClose
    , setOpen
    , setQuickOpen
    , setAttributes
    , menu, surfaceAnchor
    ) where

{-| A menu displays a list of choices on a temporary surface. They appear when
users interact with a button, action, or other control.


# Table of Contents

  - [Resources](#resources)
  - [Basic usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Menu](#menu)
  - [Quick-opening menu](#quick-opening-menu)


# Resources

  - [Demo: Menus](https://aforemny.github.io/material-components-web-elm/#menu)
  - [Material Design Guidelines: Menus](https://material.io/go/design-menus)
  - [MDC Web: Menu](https://github.com/material-components/material-components-web/tree/master/packages/mdc-menu)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-menu#sass-mixins)


# Basic usage

A menu is usually tied to an element that opens it, such as a button. For
positioning, wrap the button and the menu within an element that sets the
`surfaceAnchor` attribute. The menu's items are simply a
[list](Material-Array).

    import HalogenMWC.Button as Button
    import HalogenMWC.Array as Array
    import HalogenMWC.ArrayItem as ArrayItem
    import HalogenMWC.Menu as Menu

    data Msg
        = MenuOpened
        | MenuClosed

    main =
        Html.div [ Menu.surfaceAnchor ]
            [ Button.text
                (Button.config |> Button.setOnClick MenuOpened)
                "Open menu"
            , Menu.menu
                (Menu.config
                    |> Menu.setOpen True
                    |> Menu.setOnClose MenuClosed
                )
                [ Array.list
                    (Array.config |> Array.setWrapFocus True)
                    (ArrayItem.listItem ArrayItem.config
                        [ text "Menu item" ]
                    )
                    [ ArrayItem.listItem ArrayItem.config
                        [ text "Menu item" ]
                    ]
                ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClose
@docs setOpen
@docs setQuickOpen
@docs setAttributes


# Menu

@docs menu, surfaceAnchor


# Quick-opening menu

A menu may not show a transition when opening by setting its `setQuickOpen`
configuration option to `True`.

    Menu.menu (Menu.config |> Menu.setQuickOpen True) []

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


{-| Configuration of a menu
-}
type Config r i
    = Config
        { open :: Boolean
        , quickOpen :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe r i
        }


{-| Default configuration of a menu
-}
config :: Config r i
config =
    Config
        { open = False
        , quickOpen = False
        , additionalAttributes = []
        , onClose = Nothing
        }


{-| Specify whether a menu is open
-}
setOpen :: Boolean -> Config r i -> Config r i
setOpen open (Config config_) =
    Config { config_ | open = open }


{-| Specify whether a menu is _opening quickly_

A quickly opening menu opens without showing an animation.

-}
setQuickOpen :: Boolean -> Config r i -> Config r i
setQuickOpen quickOpen (Config config_) =
    Config { config_ | quickOpen = quickOpen }


{-| Specify a message when the user closes the menu
-}
setOnClose :: r i -> Config r i -> Config r i
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Menu view function
-}
menu :: Config r i -> Array (Html r i) -> Html r i
menu ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-menu"
        (Array.filterMap identity
            [ rootCs
            , openProp config_
            , quickOpenProp config_
            , closeHandler config_
            ]
            ++ additionalAttributes
        )
        nodes


{-| Menu surface anchor attribute

Use this on a HTML element that contains both the triggering element and the
menu itself.

-}
surfaceAnchor :: Html.Attribute r i
surfaceAnchor =
    class "mdc-menu-surface--anchor"


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-menu mdc-menu-surface")


openProp :: Config r i -> Maybe (Html.Attribute r i)
openProp (Config { open }) =
    Just (Html.Attributes.property "open" (Encode.bool open))


quickOpenProp :: Config r i -> Maybe (Html.Attribute r i)
quickOpenProp (Config { quickOpen }) =
    Just (Html.Attributes.property "quickOpen" (Encode.bool quickOpen))


closeHandler :: Config r i -> Maybe (Html.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (Html.Events.on "MDCMenu:close" << Decode.succeed) onClose
