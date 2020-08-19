module HalogenMWC.Ripple
    ( Config, config
    , setColor
    , setAttributes
    , bounded
    , unbounded
    , Color, primary, accent
    ) where

{-| Material “ink ripple” interaction effect.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Bounded Ripple](#bounded-ripple)
  - [Unbounded Ripple](#unbounded-ripple)
  - [Colored Ripple](#colored-ripple)


# Resources

  - [Demo: Ripples](https://aforemny.github.io/material-components-web-elm/#ripple)
  - [Material Design Guidelines: States](https://material.io/go/design-states)
  - [MDC Web: Ripple](https://github.com/material-components/material-components-web/tree/master/packages/mdc-ripple)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-ripple#sass-apis)


# Basic Usage

Ripples come in two variants. Use `bounded` for bounded ripple effects which
work best when used for contained surfaces, and `unbounded` for unbounded
ripple effects which work best with icons.

    import HalogenMWC.Ripple as Ripple

    main =
        Html.div []
            [ text "Click me!"
            , Ripple.bounded Ripple.config
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setColor
@docs setAttributes


# Bounded Ripple

@docs bounded


# Unbounded Ripple

    Html.span []
        [ text "🙌"
        , Ripple.unbounded Ripple.config
        ]

@docs unbounded


# Colored Ripple

If you want to set the ripple effect to either primary or accent color, use its
`setColor` configuration option and specify a `Color`.

    Ripple.bounded
        (Ripple.config |> Ripple.setColor (Just Ripple.primary))

@docs Color, primary, accent

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




{-| Ripple configuration
-}
type Config r i
    =
        { color :: Maybe Color
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default ripple configuration
-}
config :: Config r i
config =
    Config
        { color = Nothing
        , additionalAttributes = []
        }


{-| Specify a ripple effect's color
-}
setColor :: Maybe Color -> Config r i -> Config r i
setColor color (Config config_) =
    Config { config_ | color = color }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Ripple effect's color
-}
data Color
    = Primary
    | Accent


{-| Primary variant of a ripple effect's color
-}
primary :: Color
primary =
    Primary


{-| Accent variant of a ripple effect's color
-}
accent :: Color
accent =
    Accent


ripple :: Boolean -> Config r i -> Html r i
ripple isUnbounded ((Config { additionalAttributes }) as config_) =
    Html.node "mdc-ripple"
        (Array.filterMap identity
            [ unboundedProp isUnbounded
            , unboundedData isUnbounded
            , colorCs config_
            , rippleSurface
            , Just (style "position" "absolute")
            , Just (style "top" "0")
            , Just (style "left" "0")
            , Just (style "right" "0")
            , Just (style "bottom" "0")
            ]
            ++ additionalAttributes
        )
        []


{-| Bounded ripple view function
-}
bounded :: Config r i -> Html r i
bounded =
    ripple False


{-| Unbounded ripple view function
-}
unbounded :: Config r i -> Html r i
unbounded =
    ripple True


rippleSurface :: Maybe (Html.Attribute r i)
rippleSurface =
    Just (class "mdc-ripple-surface")


colorCs :: Config r i -> Maybe (Html.Attribute r i)
colorCs (Config { color }) =
    case color of
        Just Primary ->
            Just (class "mdc-ripple-surface--primary")

        Just Accent ->
            Just (class "mdc-ripple-surface--accent")

        Nothing ->
            Nothing


unboundedProp :: Boolean -> Maybe (Html.Attribute r i)
unboundedProp isUnbounded =
    Just (Html.Attributes.property "unbounded" (Encode.bool isUnbounded))


unboundedData :: Boolean -> Maybe (Html.Attribute r i)
unboundedData isUnbounded =
    if isUnbounded then
        Just (Html.Attributes.attribute "data-mdc-ripple-is-unbounded" "")

    else
        Nothing
