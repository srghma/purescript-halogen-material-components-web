module HalogenMWC.Drawer.Permanent
    ( Config, config
    , setAttributes
    , drawer, content
    , header, title, subtitle
    ) where

{-| The drawer is used to organize access to destinations and
other functionality on an app.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Permanent Drawer](#permanent-drawer)
  - [Drawer with Header](#drawer-with-header)


# Resources

  - [Demo: Drawers](https://aforemny.github.io/material-components-web-elm/#drawer)
  - [Material Design Guidelines: Navigation Drawer](https://material.io/go/design-navigation-drawer)
  - [MDC Web: Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-drawer)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-drawer#sass-mixins)


# Basic Usage

    import Html (Html, text)
    import Html.Attributes (style)
    import Material.Drawer.Permanent as PermanentDrawer
    import Material.Array as Array
    import Material.ArrayItem as ArrayItem

    main =
        Html.div
            [ style "display" "flex"
            , style "flex-flow" "row nowrap"
            ]
            [ PermanentDrawer.drawer PermanentDrawer.config
                [ PermanentDrawer.content []
                    [ Array.list Array.config
                        [ ArrayItem.listItem ArrayItem.config
                            [ text "Home" ]
                        , ArrayItem.listItem ArrayItem.config
                            [ text "Log out" ]
                        ]
                    ]
                ]
            , Html.div [] [ text "Main Content" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setAttributes


# Permanent Drawer

@docs drawer, content


# Drawer with Header

Drawers can contain a header element which will not scroll with the rest of the
drawer content. Things like account switchers and titles should live in the
header element.

@docs header, title, subtitle

-}

import Html (Html)
import Html.Attributes (class)


{-| Configuration of a permanent drawer
-}
type Config r i
    = Config { additionalAttributes :: Array (IProp r i) }


{-| Default configuration of a permanent drawer
-}
config :: Config r i
config =
    Config { additionalAttributes = [] }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Permanent drawer view function
-}
drawer :: Config r i -> Array (Html r i) -> Html r i
drawer (Config { additionalAttributes }) nodes =
    Html.div
        (Array.filterMap identity [ rootCs ] ++ additionalAttributes)
        nodes


{-| Drawer content
-}
content :: Array (IProp r i) -> Array (Html r i) -> Html r i
content attributes nodes =
    Html.div (class "mdc-drawer__content" :: attributes) nodes


{-| Drawer header view function

    PermanentDrawer.drawer PermanentDrawer.config
        [ PermanentDrawer.header []
            [ Html.h3 [ PermanentDrawer.title ]
                [ text "Title" ]
            , Html.h6 [ PermanentDrawer.subtitle ]
                [ text "Subtitle" ]
            ]
        , PermanentDrawer.content [] []
        ]

-}
header :: Array (IProp r i) -> Array (Html r i) -> Html r i
header additionalAttributes nodes =
    Html.div (class "mdc-drawer__header" :: additionalAttributes) nodes


{-| Attribute to mark the title text element of the drawer header
-}
title :: Html.Attribute r i
title =
    class "mdc-drawer__title"


{-| Attribute to mark the subtitle text element of the drawer header
-}
subtitle :: Html.Attribute r i
subtitle =
    class "mdc-drawer__subtitle"


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-drawer")
