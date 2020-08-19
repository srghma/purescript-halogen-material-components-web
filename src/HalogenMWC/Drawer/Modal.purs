module HalogenMWC.Drawer.Modal
    ( Config, config
    
    
    
    , drawer, content
    , scrim
    , header, title, subtitle
    ) where

{-| The drawer is used to organize access to destinations and
other functionality on an app.

Modal drawers are elevated above most of the app's UI and don't affect the
screen's layout grid.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Modal Drawer](#modal-drawer)
  - [Drawer with Header](#drawer-with-header)


# Resources

  - [Demo: Drawers](https://aforemny.github.io/material-components-web-elm/#drawer)
  - [Material Design Guidelines: Navigation Drawer](https://material.io/go/design-navigation-drawer)
  - [MDC Web: Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-drawer)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-drawer#sass-mixins)


# Basic Usage

    import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
    
    import HalogenMWC.Drawer.Modal as ModalDrawer
    import HalogenMWC.Array as Array
    import HalogenMWC.ArrayItem as ArrayItem

    main =
        Html.div
            [ style "display" "flex"
            , style "flex-flow" "row nowrap"
            ]
            [ ModalDrawer.drawer ModalDrawer.config
                [ ModalDrawer.content []
                    [ Array.list Array.config
                        [ ArrayItem.listItem ArrayItem.config
                            [ text "Home" ]
                        , ArrayItem.listItem ArrayItem.config
                            [ text "Log out" ]
                        ]
                    ]
                ]
            , ModalDrawer.scrim [] []
            , Html.div [] [ text "Main Content" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClose
@docs setOpen
@docs setAttributes


# Modal Drawer

@docs drawer, content
@docs scrim


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







type Config r i
    =
        { open :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe r i
        }



config :: Config r i
config =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }



setOpen :: Boolean -> Config r i -> Config r i
setOpen open (Config config_) =
    Config { config_ | open = open }



setOnClose :: r i -> Config r i -> Config r i
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



drawer :: Config r i -> Array (Html r i) -> Html r i
drawer ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-drawer"
        (Array.filterMap identity
            [ rootCs
            , modalCs
            , openProp config_
            , closeHandler config_
            ]
            ++ additionalAttributes
        )
        nodes



content :: Array (IProp r i) -> Array (Html r i) -> Html r i
content attributes nodes =
    Html.div (class "mdc-drawer__content" :: attributes) nodes



header :: Array (IProp r i) -> Array (Html r i) -> Html r i
header additionalAttributes nodes =
    Html.div (class "mdc-drawer__header" :: additionalAttributes) nodes



title :: Html.Attribute r i
title =
    class "mdc-drawer__title"



subtitle :: Html.Attribute r i
subtitle =
    class "mdc-drawer__subtitle"


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-drawer")


modalCs :: Maybe (Html.Attribute r i)
modalCs =
    Just (class "mdc-drawer--modal")


openProp :: Config r i -> Maybe (Html.Attribute r i)
openProp (Config { open }) =
    Just (Html.Attributes.property "open" (Encode.bool open))


closeHandler :: Config r i -> Maybe (Html.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (Html.Events.on "MDCDrawer:close" << Decode.succeed) onClose



scrim :: Array (IProp r i) -> Array (Html r i) -> Html r i
scrim additionalAttributes nodes =
    Html.div (class "mdc-drawer-scrim" :: additionalAttributes) nodes
