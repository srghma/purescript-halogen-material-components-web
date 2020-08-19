module HalogenMWC.Slider
    ( Config, config









    , slider
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import Svg
import Svg.Attributes



-- TODO: Prevent FOUC



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



setMin :: Maybe Float -> Config r i -> Config r i
setMin min (Config config_) =
    Config { config_ | min = min }



setMax :: Maybe Float -> Config r i -> Config r i
setMax max (Config config_) =
    Config { config_ | max = max }



setStep :: Maybe Float -> Config r i -> Config r i
setStep step (Config config_) =
    Config { config_ | step = step }



setValue :: Maybe Float -> Config r i -> Config r i
setValue value (Config config_) =
    Config { config_ | value = value }



setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnInput :: (Float -> r i) -> Config r i -> Config r i
setOnInput onInput (Config config_) =
    Config { config_ | onInput = Just onInput }



slider :: Config r i -> Html r i
slider ((Config { additionalAttributes }) as config_) =
    HH.node "mdc-slider"
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


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_slider)


displayCss :: Maybe (HH.Attribute r i)
displayCss =
    Just (style "display" "block")


discreteCs :: Config r i -> Maybe (HH.Attribute r i)
discreteCs (Config { discrete }) =
    if discrete then
        Just (HP.class_ mdc_slider____discrete)

    else
        Nothing


displayMarkersCs :: Config r i -> Maybe (HH.Attribute r i)
displayMarkersCs (Config { discrete, displayMarkers }) =
    if discrete && displayMarkers then
        Just (HP.class_ "mdc-slider--display-markers")

    else
        Nothing


tabIndexProp :: Maybe (HH.Attribute r i)
tabIndexProp =
    Just (HH.Attributes.tabindex 0)


sliderRoleAttr :: Maybe (HH.Attribute r i)
sliderRoleAttr =
    Just (HH.Attributes.attribute "role" "slider")


valueProp :: Config r i -> Maybe (HH.Attribute r i)
valueProp (Config { value }) =
    Maybe.map (HH.Attributes.property "value" << Encode.float) value


minProp :: Config r i -> Maybe (HH.Attribute r i)
minProp (Config { min }) =
    Maybe.map (HH.Attributes.property "min" << Encode.float) min


maxProp :: Config r i -> Maybe (HH.Attribute r i)
maxProp (Config { max }) =
    Maybe.map (HH.Attributes.property "max" << Encode.float) max


stepProp :: Config r i -> Maybe (HH.Attribute r i)
stepProp (Config { step }) =
    Maybe.map (HH.Attributes.property "step" << Encode.float) step


disabledProp :: Config r i -> Maybe (HH.Attribute r i)
disabledProp (Config { disabled }) =
    Just (HH.Attributes.property "disabled" (Encode.bool disabled))


ariaValueMinAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaValueMinAttr (Config { min }) =
    Maybe.map (HH.Attributes.attribute "aria-valuemin" << String.fromFloat) min


ariaValueMaxAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaValueMaxAttr (Config { max }) =
    Maybe.map (HH.Attributes.attribute "aria-valuemax" << String.fromFloat) max


ariaValuenowAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaValuenowAttr (Config { value }) =
    Maybe.map (HH.Attributes.attribute "aria-valuenow" << String.fromFloat) value


changeHandler :: Config r i -> Maybe (HH.Attribute r i)
changeHandler (Config { onInput }) =
    Maybe.map
        (\handler ->
            HH.Events.on "MDCSlider:input"
                (Decode.map handler (Decode.at [ "target", "value" ] Decode.float))
        )
        onInput


trackContainerElt :: Html r i
trackContainerElt =
    HH.div [ HP.class_ mdc_slider__track_container ] [ trackElt, trackMarkerContainerElt ]


trackElt :: Html r i
trackElt =
    HH.div [ HP.class_ mdc_slider__track ] []


trackMarkerContainerElt :: Html r i
trackMarkerContainerElt =
    HH.div [ HP.class_ mdc_slider__track_marker_container ] []


thumbContainerElt :: Config r i -> Html r i
thumbContainerElt (Config { discrete }) =
    HH.div [ HP.class_ mdc_slider__thumb_container ]
        (if discrete then
            [ pinElt, thumbElt, focusRingElt ]

         else
            [ thumbElt, focusRingElt ]
        )


pinElt :: Html r i
pinElt =
    HH.div [ HP.class_ mdc_slider__pin ] [ pinValueMarkerElt ]


pinValueMarkerElt :: Html r i
pinValueMarkerElt =
    HH.div [ HP.class_ mdc_slider__pin_value_marker ] []


thumbElt :: Html r i
thumbElt =
    Svg.svg
        [ Svg.Attributes.class_ "mdc-slider__thumb"
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
    HH.div [ HP.class_ mdc_slider__focus_ring ] []
