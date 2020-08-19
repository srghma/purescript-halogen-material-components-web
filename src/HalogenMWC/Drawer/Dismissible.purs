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

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode


{-| Configuration of a dismissible drawer
-}
type Config r i
    = Config
        { open :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe r i
        }


{-| Default configuration of a dismissible drawer
-}
config :: Config r i
config =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }


{-| Specify whether the drawer is open
-}
setOpen :: Boolean -> Config r i -> Config r i
setOpen open (Config config_) =
    Config { config_ | open = open }


{-| Specify message when the user closes the drawer
-}
setOnClose :: r i -> Config r i -> Config r i
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Dismissible drawer view function
-}
drawer :: Config r i -> Array (Html r i) -> Html r i
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
content :: Array (IProp r i) -> Array (Html r i) -> Html r i
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


dismissibleCs :: Maybe (Html.Attribute r i)
dismissibleCs =
    Just (class "mdc-drawer--dismissible")


openProp :: Config r i -> Maybe (Html.Attribute r i)
openProp (Config { open }) =
    Just (Html.Attributes.property "open" (Encode.bool open))


closeHandler :: Config r i -> Maybe (Html.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (Html.Events.on "MDCDrawer:close" << Decode.succeed) onClose


{-| Dismissible drawer's app content marker

Apply this attribute to the page's content for the open/close animation to
work. The page content has to be the next sibling of the dismissible drawer.

-}
appContent :: Html.Attribute r i
appContent =
    class "mdc-drawer-app-content"
