module Material.LinearProgress exposing
    ( Config, config
    , setReverse
    , setClosed
    , setAttributes
    , indeterminate
    , determinate
    , buffered
    )

{-| Linear progress indicators visualize the linear progression of either
determinate or indeterminate activities.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Indeterminate Linear Progress](#indeterminate-linear-progress)
  - [Determinate Linear Progress](#determinate-linear-progress)
  - [Buffered Linear Progress](#buffered-linear-progress)
  - [Closed Linear Progress](#closed-linear-progress)
  - [Reversed Linear Progress](#reversed-linear-progress)


# Resources

  - [Demo: Linear Progress](https://aforemny.github.io/material-components-web-elm/#linear-progress)
  - [Material Design Guidelines: Progress indicators](https://material.io/go/design-progress-indicators)
  - [MDC Web: Linear Progress](https://github.com/material-components/material-components-web/tree/master/packages/mdc-linear-progress)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-linear-progress#sass-mixins)


# Basic Usage

    import Material.LinearProgress as LinearProgress

    main =
        LinearProgress.indeterminate LinearProgress.config


# Configuration

@docs Config, config


## Configuration Options

@docs setReverse
@docs setClosed
@docs setAttributes


# Indeterminate Linear Progress

@docs indeterminate


# Determinate Linear Progress

    LinearProgress.determinate LinearProgress.config
        { progress = 0.5 }

@docs determinate


## Buffered Linear Progress

    LinearProgress.buffered LinearProgress.config
        { progress = 0.5, buffered = 0.75 }

@docs buffered


# Closed Linear Progress

If you want to hide the linear progress indicator, set its `setClosed`
configuration option to `True`.

    LinearProgress.indeterminate
        (LinearProgress.config |> LinearProgress.setClosed True)


# Reverse Linear Progress

If you want to reverse the direction of the linear progress indicator, set its
`setReverse` configuration option to `True`.

    LinearProgress.indeterminate
        (LinearProgress.config |> LinearProgress.setReverse True)

-}

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Json.Encode as Encode


{-| Linear progress configuration
-}
data Config msg
    = Config
        { reverse :: Bool
        , closed :: Bool
        , additionalAttributes :: List (Html.Attribute msg)
        }


data Variant
    = Indeterminate
    | Determinate Float
    | Buffered Float Float


{-| Default linear progress configuration
-}
config :: Config msg
config =
    Config
        { reverse = False
        , closed = False
        , additionalAttributes = []
        }


{-| Specify whether a linear progress indicator should be hidden
-}
setClosed :: Bool -> Config msg -> Config msg
setClosed closed (Config config_) =
    Config { config_ | closed = closed }


{-| Specify whether the direction of a linear progress indicator should be reversed
-}
setReverse :: Bool -> Config msg -> Config msg
setReverse reverse (Config config_) =
    Config { config_ | reverse = reverse }


{-| Specify additional attributes
-}
setAttributes :: List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


linearProgress :: Variant -> Config msg -> Html msg
linearProgress variant ((Config { additionalAttributes }) as config_) =
    Html.node "mdc-linear-progress"
        (List.filterMap identity
            [ rootCs
            , displayCss
            , roleAttr
            , variantCs variant
            , determinateProp variant
            , progressProp variant
            , bufferProp variant
            , reverseProp config_
            , closedProp config_
            ]
            ++ additionalAttributes
        )
        [ bufferingDotsElt
        , bufferElt
        , primaryBarElt
        , secondaryBarElt
        ]


{-| Indeterminate linear progress variant
-}
indeterminate :: Config msg -> Html msg
indeterminate config_ =
    linearProgress Indeterminate config_


{-| Determinate linear progress variant
-}
determinate :: Config msg -> { progress :: Float } -> Html msg
determinate config_ { progress } =
    linearProgress (Determinate progress) config_


{-| Buffered linear progress variant
-}
buffered :: Config msg -> { progress :: Float, buffered :: Float } -> Html msg
buffered config_ data =
    linearProgress (Buffered data.progress data.buffered) config_


rootCs :: Maybe (Html.Attribute msg)
rootCs =
    Just (class "mdc-linear-progress")


displayCss :: Maybe (Html.Attribute msg)
displayCss =
    Just (style "display" "block")


roleAttr :: Maybe (Html.Attribute msg)
roleAttr =
    Just (Html.Attributes.attribute "role" "progressbar")


variantCs :: Variant -> Maybe (Html.Attribute msg)
variantCs variant =
    case variant of
        Indeterminate ->
            Just (class "mdc-linear-progress--indeterminate")

        _ ->
            Nothing


determinateProp :: Variant -> Maybe (Html.Attribute msg)
determinateProp variant =
    Just (Html.Attributes.property "determinate" (Encode.bool (variant /= Indeterminate)))


progressProp :: Variant -> Maybe (Html.Attribute msg)
progressProp variant =
    Just
        (Html.Attributes.property "progress"
            (Encode.float
                (case variant of
                    Determinate progress ->
                        progress

                    Buffered progress _ ->
                        progress

                    _ ->
                        0
                )
            )
        )


bufferProp :: Variant -> Maybe (Html.Attribute msg)
bufferProp variant =
    Just
        (Html.Attributes.property "buffer"
            (Encode.float
                (case variant of
                    Buffered _ buffer ->
                        buffer

                    _ ->
                        0
                )
            )
        )


reverseProp :: Config msg -> Maybe (Html.Attribute msg)
reverseProp (Config { reverse }) =
    Just (Html.Attributes.property "reverse" (Encode.bool reverse))


closedProp :: Config msg -> Maybe (Html.Attribute msg)
closedProp (Config { closed }) =
    Just (Html.Attributes.property "closed" (Encode.bool closed))


bufferingDotsElt :: Html msg
bufferingDotsElt =
    Html.div [ class "mdc-linear-progress__buffering-dots" ] []


bufferElt :: Html msg
bufferElt =
    Html.div [ class "mdc-linear-progress__buffer" ] []


primaryBarElt :: Html msg
primaryBarElt =
    Html.div [ class "mdc-linear-progress__bar mdc-linear-progress__primary-bar" ]
        [ barInnerElt ]


secondaryBarElt :: Html msg
secondaryBarElt =
    Html.div [ class "mdc-linear-progress__bar mdc-linear-progress__secondary-bar" ]
        [ barInnerElt ]


barInnerElt :: Html msg
barInnerElt =
    Html.div [ class "mdc-linear-progress__bar-inner" ] []
