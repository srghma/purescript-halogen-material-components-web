module HalogenMWC.Tab
    ( Config, config
    , setOnClick
    , setActive
    , setAttributes
    , Tab, tab, Content
    ) where

{-| Tabs organize and allow navigation between groups of content that are
related and at the same level of hierarchy. The tab bar contains the tab
components.

This module concerns the tab items. If you are looking for information about
the tab bar container, refer to [Material.TabBar](Material-TabBar).


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Tab](#tab)
  - [Active Tab](#active-tab)


# Resources

  - [Demo: Tab Bar](https://aforemny.github.io/material-components-web-elm/#tabbar)
  - [Material Design Guidelines: Tabs](https://material.io/go/design-tabs)
  - MDC Web:
    [Tab Bar](https://github.com/material-components/material-components-web/tree/master/packages/mdc-tab-bar),
    [Tab](https://github.com/material-components/material-components-web/tree/master/packages/mdc-tab),
    [Tab Scroller](https://github.com/material-components/material-components-web/tree/master/packages/mdc-tab-scroller)
  - Sass Mixins:
    [Tab Bar](https://github.com/material-components/material-components-web/tree/master/packages/mdc-tab-bar#sass-mixins),
    [Tab](https://github.com/material-components/material-components-web/tree/master/packages/mdc-tab#sass-mixins),
    [Tab Scroller](https://github.com/material-components/material-components-web/tree/master/packages/mdc-tab-scroller#sass-mixins)


# Basic Usage

    import Material.Tab as Tab
    import Material.TabBar as TabBar

    data Msg
        = TabClicked Int

    main =
        TabBar.tabBar TabBar.config
            [ Tab.tab
                (Tab.config
                    |> Tab.setActive True
                    |> Tab.setOnClick (TabClicked 0)
                )
                { label = "Tab 1", icon = Nothing }
            , Tab.tab
                (Tab.config |> Tab.setOnClick (TabClicked 1))
                { label = "Tab 2", icon = Nothing }
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClick
@docs setActive
@docs setAttributes


# Tab

@docs Tab, tab, Content


# Active Tab

To mark a tab as active, set its `setActive` configuration option to `True`.

    Tab.tab (Tab.config |> Tab.setActive True)

-}

import Html
import Material.Tab.Internal (Config(..), Tab(..))


{-| Configuration of a tab
-}
type Config r i =
    Material.Tab.Internal.Config r i


{-| Default configuration of a tab
-}
config :: Config r i
config =
    Config
        { active = False
        , additionalAttributes = []
        , onClick = Nothing
        , content = { label = "", icon = Nothing }
        }


{-| Specify a message when the user clicks a tab
-}
setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }


{-| Specify whether the tab is active
-}
setActive :: Boolean -> Config r i -> Config r i
setActive active (Config config_) =
    Config { config_ | active = active }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Content of a tab
-}
data Content =
    { label :: String
    , icon :: Maybe String
    }


{-| Tab type

Tabs can only be rendered within a [tab bar](Material-TabBar).

-}
data Tab r i =
    Material.Tab.Internal.Tab r i


{-| Tab constructor
-}
tab :: Config r i -> Content -> Tab r i
tab (Config config_) content =
    Tab (Config { config_ | content = content })
