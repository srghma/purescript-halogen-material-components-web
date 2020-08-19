module HalogenMWC.Drawer.Dismissible
    ( Config, config
    , setOnClose
    , setOpen
    , setAttributes
    , drawer, content
    , appContent
    , header, title, subtitle
    ) where

{-| The drawer is used to organize access to destinations and
other functionality on an app.

Dismissible drawers are by default hidden off screen, and can slide into view.
Dismissible drawers should be used when navigation is not common, and the main
app content is prioritized.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Dismissible Drawer](#dismissible-drawer)
  - [Drawer with Header](#drawer-with-header)


# Resources

  - [Demo: Drawers](https://aforemny.github.io/material-components-web-elm/#drawer)
  - [Material Design Guidelines: Navigation Drawer](https://material.io/go/design-navigation-drawer)
  - [MDC Web: Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-drawer)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-drawer#sass-mixins)


# Basic Usage

    import Html (Html, text)
    import Html.Attributes (style)
    import Material.Drawer.Dismissible as DismissibleDrawer
    import Material.Array as Array
    import Material.ArrayItem as ArrayItem

    main =
        Html.div
            [ style "display" "flex"
            , style "flex-flow" "row nowrap"
            ]
            [ DismissibleDrawer.drawer DismissibleDrawer.config
                [ DismissibleDrawer.content []
                    [ Array.list Array.config
                        [ ArrayItem.listItem ArrayItem.config
                            [ text "Home" ]
                        , ArrayItem.listItem ArrayItem.config
                            [ text "Log out" ]
                        ]
                    ]
                ]
            , Html.div [ DismissibleDrawer.appContent ]
                [ text "Main Content" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClose
@docs setOpen
@docs setAttributes


# Dismissible Drawer

@docs drawer, content
@docs appContent


# Drawer with Header

Drawers can contain a header element which will not scroll with the rest of the
drawer content. Things like account switchers and titles should live in the
header element.

@docs header, title, subtitle

-}

import Html (Html)
import Html.Attributes (class)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode


{-| Configuration of a dismissible drawer
-}
type Config r i
    = Config
        { open :: Bool
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe msg
        }


{-| Default configuration of a dismissible drawer
-}
config :: Config msg
config =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }


{-| Specify whether the drawer is open
-}
setOpen :: Bool -> Config msg -> Config msg
setOpen open (Config config_) =
    Config { config_ | open = open }


{-| Specify message when the user closes the drawer
-}
setOnClose :: msg -> Config msg -> Config msg
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Dismissible drawer view function
-}
drawer :: Config msg -> Array (Html msg) -> Html msg
drawer ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-drawer"
        (Array.filterMap identity
            [ rootCs
            , dismissibleCs
            , openProp config_
            , closeHandler config_
            ]
            ++ additionalAttributes
        )
        nodes


{-| Drawer content
-}
content :: Array (IProp r i) -> Array (Html msg) -> Html msg
content attributes nodes =
    Html.div (class "mdc-drawer__content" :: attributes) nodes


{-| Drawer header view function

    DismissibleDrawer.drawer DismissibleDrawer.config
        [ DismissibleDrawer.header []
            [ Html.h3 [ DismissibleDrawer.title ]
                [ text "Title" ]
            , Html.h6 [ DismissibleDrawer.subtitle ]
                [ text "Subtitle" ]
            ]
        , DismissibleDrawer.content [] []
        ]

-}
header :: Array (IProp r i) -> Array (Html msg) -> Html msg
header additionalAttributes nodes =
    Html.div (class "mdc-drawer__header" :: additionalAttributes) nodes


{-| Attribute to mark the title text element of the drawer header
-}
title :: Html.Attribute msg
title =
    class "mdc-drawer__title"


{-| Attribute to mark the subtitle text element of the drawer header
-}
subtitle :: Html.Attribute msg
subtitle =
    class "mdc-drawer__subtitle"


rootCs :: Maybe (Html.Attribute msg)
rootCs =
    Just (class "mdc-drawer")


dismissibleCs :: Maybe (Html.Attribute msg)
dismissibleCs =
    Just (class "mdc-drawer--dismissible")


openProp :: Config msg -> Maybe (Html.Attribute msg)
openProp (Config { open }) =
    Just (Html.Attributes.property "open" (Encode.bool open))


closeHandler :: Config msg -> Maybe (Html.Attribute msg)
closeHandler (Config { onClose }) =
    Maybe.map (Html.Events.on "MDCDrawer:close" << Decode.succeed) onClose


{-| Dismissible drawer's app content marker

Apply this attribute to the page's content for the open/close animation to
work. The page content has to be the next sibling of the dismissible drawer.

-}
appContent :: Html.Attribute msg
appContent =
    class "mdc-drawer-app-content"
