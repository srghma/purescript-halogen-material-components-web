module HalogenMWC.Slider
    ( Config, config
    
    
    
    
    
    
    
    
    
    , slider
    ) where

{-| Slider provides a component to select a numerical value within a range.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Continuous Slider](#continuous-slider)
  - [Custom range values](#using-a-step-value)
  - [Using a step value](#using-a-step-value)
  - [Disabled Slider](#disabled-slider)
  - [Discrete Slider](#discrete-slider)
      - [Track Markers](#track-markers)
  - [Focus a Slider](#focus-a-slider)


# Resources

  - [Demo: Sliders](https://aforemny.github.io/material-components-web-elm/#slider)
  - [Material Design Guidelines: Sliders](https://material.io/go/design-sliders)
  - [MDC Web: Slider](https://github.com/material-components/material-components-web/tree/master/packages/mdc-slider)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-slider#sass-mixins)


# Basic Usage

    import HalogenMWC.Slider as Slider

    data Msg
        = ValueChanged Float

    main =
        Slider.slider
            (Slider.config
                |> Slider.setValue (Just 50)
                |> Slider.setOnInput ValueChanged
            )


# Configurations

@docs Config, config


## Configuration Options

@docs setOnInput
@docs setDiscrete
@docs setDisplayMarkers
@docs setMin
@docs setMax
@docs setStep
@docs setValue
@docs setDisabled
@docs setAttributes


# Continuous Slider

@docs slider


# Custom range values

To set a custom range, use the slider's `setMin` and `setMax` configuration
options.

    Slider.slider
        (Slider.config
            |> Slider.setMin (Just 0)
            |> Slider.setMax (Just 100)
        )


# Using a step value

To allow for quantization of the user input, use the slider's `setStep`
configuration option.

    Slider.slider (Slider.config |> Slider.setStep (Just 4.5))


# Disabled Slider

To disable a slider its `setDisabled` configuration option to `True`.

Disabled sliders cannot be interacted with and have no visual interaction
effect.

    Slider.slider (Slider.config |> Slider.setDisabled True)


# Discrete Slider

To treat a slider as a discrete slider its `setDiscrete` configuration
option to `True`.

    Slider.slider (Slider.config |> Slider.setDiscrete True)


## Track Markers

To have a discrete slider show track markers its `setDisplayMarkers`
configuration option to `True`.

Note that non-discrete sliders ignore this configuration option.

    Slider.slider
        (Slider.config |> Slider.setDisplayMarkers True)


# Focus a Slider

You may programatically focus a slider by assigning an id attribute to it and
use `Browser.Dom.focus`.

    Slider.slider
        (Slider.config
            |> Slider.setAttributes
                [ Html.Attributes.id "my-slider" ]
        )

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import Svg
import Svg.Attributes



-- TODO: Prevent FOUC


{-| Configuration of a slider
-}
type Config r i
    =
        { discrete :: Boolean
        , displayMarkers :: Boolean
        , min :: Maybe Float
        , max :: Maybe Float
        , step :: Maybe Float
        , value :: Maybe Float
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onInput :: Maybe (Float -> r i)
        }


{-| Default configuration of a slider
-}
config :: Config r i
config =
    Config
        { discrete = False
        , displayMarkers = False
        , min = Nothing
        , max = Nothing
        , step = Nothing
        , value = Nothing
        , disabled = False
        , additionalAttributes = []
        , onInput = Nothing
        }


{-| Specify whether a slider is _discrete_

Discrete sliders feature a pin that indicates the current value while
interacting with the slider.

This works best for integer-valued sliders, but this is not a requirement.

-}
setDiscrete :: Boolean -> Config r i -> Config r i
setDiscrete discrete (Config config_) =
    Config { config_ | discrete = discrete }


{-| Specify whether a slider should display markers

Note that this option is ignored by non-discrete sliders.

-}
setDisplayMarkers :: Boolean -> Config r i -> Config r i
setDisplayMarkers displayMarkers (Config config_) =
    Config { config_ | displayMarkers = displayMarkers }


{-| Specify a slider's minimum value
-}
setMin :: Maybe Float -> Config r i -> Config r i
setMin min (Config config_) =
    Config { config_ | min = min }


{-| Specify a slider's maximum value
-}
setMax :: Maybe Float -> Config r i -> Config r i
setMax max (Config config_) =
    Config { config_ | max = max }


{-| Specify a slider's step value
-}
setStep :: Maybe Float -> Config r i -> Config r i
setStep step (Config config_) =
    Config { config_ | step = step }


{-| Specify a slider's value
-}
setValue :: Maybe Float -> Config r i -> Config r i
setValue value (Config config_) =
    Config { config_ | value = value }


{-| Specify whether a slider is disabled

Disabled sliders canot be interacted with and have no visual interaction
effect.

-}
setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user interacts with the slider
-}
setOnInput :: (Float -> r i) -> Config r i -> Config r i
setOnInput onInput (Config config_) =
    Config { config_ | onInput = Just onInput }


{-| Slider view function
-}
slider :: Config r i -> Html r i
slider ((Config { additionalAttributes }) as config_) =
    Html.node "mdc-slider"
        (Array.filterMap identity
            [ rootCs
            , displayCss
            , discreteCs config_
            , displayMarkersCs config_
            , tabIndexProp
            , sliderRoleAttr
            , valueProp config_
            , minProp config_
            , maxProp config_
            , stepProp config_
            , disabledProp config_
            , ariaValueMinAttr config_
            , ariaValueMaxAttr config_
            , ariaValuenowAttr config_
            , changeHandler config_
            ]
            ++ additionalAttributes
        )
        [ trackContainerElt
        , thumbContainerElt config_
        ]


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-slider")


displayCss :: Maybe (Html.Attribute r i)
displayCss =
    Just (style "display" "block")


discreteCs :: Config r i -> Maybe (Html.Attribute r i)
discreteCs (Config { discrete }) =
    if discrete then
        Just (class "mdc-slider--discrete")

    else
        Nothing


displayMarkersCs :: Config r i -> Maybe (Html.Attribute r i)
displayMarkersCs (Config { discrete, displayMarkers }) =
    if discrete && displayMarkers then
        Just (class "mdc-slider--display-markers")

    else
        Nothing


tabIndexProp :: Maybe (Html.Attribute r i)
tabIndexProp =
    Just (Html.Attributes.tabindex 0)


sliderRoleAttr :: Maybe (Html.Attribute r i)
sliderRoleAttr =
    Just (Html.Attributes.attribute "role" "slider")


valueProp :: Config r i -> Maybe (Html.Attribute r i)
valueProp (Config { value }) =
    Maybe.map (Html.Attributes.property "value" << Encode.float) value


minProp :: Config r i -> Maybe (Html.Attribute r i)
minProp (Config { min }) =
    Maybe.map (Html.Attributes.property "min" << Encode.float) min


maxProp :: Config r i -> Maybe (Html.Attribute r i)
maxProp (Config { max }) =
    Maybe.map (Html.Attributes.property "max" << Encode.float) max


stepProp :: Config r i -> Maybe (Html.Attribute r i)
stepProp (Config { step }) =
    Maybe.map (Html.Attributes.property "step" << Encode.float) step


disabledProp :: Config r i -> Maybe (Html.Attribute r i)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


ariaValueMinAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaValueMinAttr (Config { min }) =
    Maybe.map (Html.Attributes.attribute "aria-valuemin" << String.fromFloat) min


ariaValueMaxAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaValueMaxAttr (Config { max }) =
    Maybe.map (Html.Attributes.attribute "aria-valuemax" << String.fromFloat) max


ariaValuenowAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaValuenowAttr (Config { value }) =
    Maybe.map (Html.Attributes.attribute "aria-valuenow" << String.fromFloat) value


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { onInput }) =
    Maybe.map
        (\handler ->
            Html.Events.on "MDCSlider:input"
                (Decode.map handler (Decode.at [ "target", "value" ] Decode.float))
        )
        onInput


trackContainerElt :: Html r i
trackContainerElt =
    Html.div [ class "mdc-slider__track-container" ] [ trackElt, trackMarkerContainerElt ]


trackElt :: Html r i
trackElt =
    Html.div [ class "mdc-slider__track" ] []


trackMarkerContainerElt :: Html r i
trackMarkerContainerElt =
    Html.div [ class "mdc-slider__track-marker-container" ] []


thumbContainerElt :: Config r i -> Html r i
thumbContainerElt (Config { discrete }) =
    Html.div [ class "mdc-slider__thumb-container" ]
        (if discrete then
            [ pinElt, thumbElt, focusRingElt ]

         else
            [ thumbElt, focusRingElt ]
        )


pinElt :: Html r i
pinElt =
    Html.div [ class "mdc-slider__pin" ] [ pinValueMarkerElt ]


pinValueMarkerElt :: Html r i
pinValueMarkerElt =
    Html.div [ class "mdc-slider__pin-value-marker" ] []


thumbElt :: Html r i
thumbElt =
    Svg.svg
        [ Svg.Attributes.class "mdc-slider__thumb"
        , Svg.Attributes.width "21"
        , Svg.Attributes.height "21"
        ]
        [ Svg.circle
            [ Svg.Attributes.cx "10.5"
            , Svg.Attributes.cy "10.5"
            , Svg.Attributes.r "7.875"
            ]
            []
        ]


focusRingElt :: Html r i
focusRingElt =
    Html.div [ class "mdc-slider__focus-ring" ] []
