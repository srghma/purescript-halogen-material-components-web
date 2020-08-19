module HalogenMWC.TopAppBar
    ( Config, config
    , setFixed
    , setDense
    , setAttributes
    , regular
    , row, section, alignEnd, alignStart
    , navigationIcon, title
    , actionItem
    , fixedAdjust
    , denseFixedAdjust
    , denseProminentFixedAdjust
    , prominentFixedAdjust
    , shortFixedAdjust
    , short
    , shortCollapsed
    , prominent
    ) where

{-| Top App Bar acts as a container for items such as application title,
navigation icon, and action items.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Top App Bar](#top-app-bar)
      - [Top App Bar with Action Items](#top-app-bar-with-action-items)
  - [Fixed Variant](#fixed-variant)
  - [Short Variant](#short-variant)
      - [Short Always Closed Variant](#short-always-closed-variant)
  - [Prominent Variant](#prominent-variant)
  - [Dense Variant](#dense-variant)


# Resources

  - [Demo: Top App Bars](https://aforemny.github.io/material-components-web-elm/#top-app-bar)
  - [Material Design Guidelines: Top App Bar](https://material.io/go/design-app-bar-top)
  - [MDC Web: Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-top-app-bar)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-top-app-bar#sass-mixins)


# Basic Usage

    import HalogenMWC.TopAppBar as TopAppBar

    main =
        TopAppBar.regular TopAppBar.config
            [ TopAppBar.row []
                [ TopAppBar.section [ TopAppBar.alignStart ]
                    [ IconButton.iconButton
                        (IconButton.config
                            |> IconButton.setAttributes
                                [ TopAppBar.navigationIcon ]
                        )
                        "menu"
                    , Html.span [ TopAppBar.title ]
                        [ text "Title" ]
                    ]
                ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setFixed
@docs setDense
@docs setAttributes


# Top App Bar

Usually a top app bar contains one row with at least one start-aligned section.
This is where you would normally place your navigation icon and title.

@docs regular
@docs row, section, alignEnd, alignStart
@docs navigationIcon, title


## Top App Bar with Action Items

A top app bar can contain action items that are placed on the opposite side of
the navigation icon. To do so, add another end-aligned section to the top app
bar's row. Do not forget to set the `actionItem` attribute on the icons.

    TopAppBar.regular TopAppBar.config
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setAttributes
                            [ TopAppBar.navigationIcon ]
                    )
                    "menu"
                , Html.span [ TopAppBar.title ]
                    [ text "Title" ]
                ]
            , TopAppBar.section [ TopAppBar.alignEnd ]
                [ IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setAttributes
                            [ TopAppBar.actionItem ]
                    )
                    "print"
                , IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setAttributes
                            [ TopAppBar.actionItem ]
                    )
                    "bookmark"
                ]
            ]
        ]

@docs actionItem


# Fixed Variant

To make a top app bar fixed to the top, set its `setFixed` configuration option
to `True`. Since a fixed top app bar would overlay the pages content, an
appropriate margin has to be applied to the page's content, called the _fixed
adjust_.

    TopAppBar.regular
        (TopAppBar.config |> TopAppBar.setFixed True)
        []

@docs fixedAdjust
@docs denseFixedAdjust
@docs denseProminentFixedAdjust
@docs prominentFixedAdjust
@docs shortFixedAdjust


# Short Variant

Short top app bars collapse to the navigation icon side when scrolled.

    TopAppBar.short TopAppBar.config []

@docs short


## Short Always Closed Variant

A short top app bar can be configured to always appear closed.

    TopAppBar.shortCollapsed TopAppBar.config []

@docs shortCollapsed


# Prominent Variant

To make a top app bar taller than the default, you may use a prominent top app bar.

    TopAppBar.prominent TopAppBar.config []

@docs prominent


# Dense Variant

To make a top app bar shorter than the default, use its `setDense`
configuration option.

    TopAppBar.regular
        (TopAppBar.config |> TopAppBar.setDense True)
        []

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA



{-| Configuration of a top app bar
-}
type Config r i
    = Config
        { dense :: Boolean
        , fixed :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


data Variant
    = Regular
    | Short
    | ShortCollapsed
    | Prominent


{-| Default configuration of a top app bar
-}
config :: Config r i
config =
    Config
        { dense = False
        , fixed = False
        , additionalAttributes = []
        }


{-| Specify whether a top app bar is dense

A dense top app bar is more compact, featuring smaller than usual margins.

-}
setDense :: Boolean -> Config r i -> Config r i
setDense dense (Config config_) =
    Config { config_ | dense = dense }


{-| Specify whether a top app bar is fixed

A fixed top app bar does not scroll away when the user is scrolling the page.

-}
setFixed :: Boolean -> Config r i -> Config r i
setFixed fixed (Config config_) =
    Config { config_ | fixed = fixed }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


genericTopAppBar :: Variant -> Config r i -> Array (Html r i) -> Html r i
genericTopAppBar variant ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-top-app-bar"
        (Array.filterMap identity
            [ rootCs
            , variantCs variant
            , denseCs config_
            , fixedCs config_
            ]
            ++ additionalAttributes
        )
        nodes


{-| Regular top app bar view function
-}
regular :: Config r i -> Array (Html r i) -> Html r i
regular config_ nodes =
    genericTopAppBar Regular config_ nodes


{-| Short top app bar view function
-}
short :: Config r i -> Array (Html r i) -> Html r i
short config_ nodes =
    genericTopAppBar Short config_ nodes


{-| Short always closed top app bar view function
-}
shortCollapsed :: Config r i -> Array (Html r i) -> Html r i
shortCollapsed config_ nodes =
    genericTopAppBar ShortCollapsed config_ nodes


{-| Prominent top app bar view function
-}
prominent :: Config r i -> Array (Html r i) -> Html r i
prominent config_ nodes =
    genericTopAppBar Prominent config_ nodes


{-| A row is the first child of a top app bar. It contains the top app bar's
`section`s.
-}
row :: Array (IProp r i) -> Array (Html r i) -> Html r i
row attributes nodes =
    Html.section ([ class "mdc-top-app-bar__row" ] ++ attributes) nodes


{-| Sections subdivide the top app bar's rows. A section may be start- or
end-aligned. Usually, the first section is start-aligned and contains the top
app bar's navigation icon and title.
-}
section :: Array (IProp r i) -> Array (Html r i) -> Html r i
section attributes nodes =
    Html.section ([ class "mdc-top-app-bar__section" ] ++ attributes) nodes


{-| Start-align a top app bar's `section`
-}
alignStart :: Html.Attribute r i
alignStart =
    class "mdc-top-app-bar__section--align-start"


{-| End-align a top app bar's `section`
-}
alignEnd :: Html.Attribute r i
alignEnd =
    class "mdc-top-app-bar__section--align-end"


{-| Apply this attribute to an icon button to mark it as a top app bar's
navigation icon
-}
navigationIcon :: Html.Attribute r i
navigationIcon =
    class "mdc-top-app-bar__navigation-icon"


{-| Apply this attribute to a element to mark it as the top app bar's title
-}
title :: Html.Attribute r i
title =
    class "mdc-top-app-bar__title"


{-| Apply this attribute to a icon button to mark it as a top app bar's action
item
-}
actionItem :: Html.Attribute r i
actionItem =
    class "mdc-top-app-bar__action-item"


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-top-app-bar")


variantCs :: Variant -> Maybe (Html.Attribute r i)
variantCs variant =
    case variant of
        Regular ->
            Nothing

        Short ->
            Just (class "mdc-top-app-bar--short")

        ShortCollapsed ->
            Just (class "mdc-top-app-bar--short mdc-top-app-bar--short-collapsed")

        Prominent ->
            Just (class "mdc-top-app-bar--prominent")


denseCs :: Config r i -> Maybe (Html.Attribute r i)
denseCs (Config { dense }) =
    if dense then
        Just (class "mdc-top-app-bar--dense")

    else
        Nothing


fixedCs :: Config r i -> Maybe (Html.Attribute r i)
fixedCs (Config { fixed }) =
    if fixed then
        Just (class "mdc-top-app-bar--fixed")

    else
        Nothing


{-| Appropriate padding for a fixed top app bar.

Apply this to the page's content so that a fixed top app bar does not overlay
the content.

-}
fixedAdjust :: Html.Attribute r i
fixedAdjust =
    class "mdc-top-app-bar--fixed-adjust"


{-| Appropriate padding for a dense fixed top app bar.

Apply this to the page's content so that a fixed top app bar does not overlay
the content.

-}
denseFixedAdjust :: Html.Attribute r i
denseFixedAdjust =
    class "mdc-top-app-bar--dense-fixed-adjust"


{-| Appropriate padding for a short fixed top app bar.

Apply this to the page's content so that a fixed top app bar does not overlay
the content.

-}
shortFixedAdjust :: Html.Attribute r i
shortFixedAdjust =
    class "mdc-top-app-bar--short-fixed-adjust"


{-| Appropriate padding for a prominent fixed top app bar.

Apply this to the page's content so that a fixed top app bar does not overlay
the content.

-}
prominentFixedAdjust :: Html.Attribute r i
prominentFixedAdjust =
    class "mdc-top-app-bar--prominent-fixed-adjust"


{-| Appropriate padding for a dense prominent fixed top app bar.

Apply this to the page's content so that a fixed top app bar does not overlay
the content.

-}
denseProminentFixedAdjust :: Html.Attribute r i
denseProminentFixedAdjust =
    class "mdc-top-app-bar--dense-prominent-fixed-adjust"
