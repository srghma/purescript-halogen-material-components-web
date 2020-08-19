module HalogenMWC.Slider where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Halogen.SVG.Elements as Halogen.SVG.Elements
import Halogen.SVG.Attributes as Halogen.SVG.Attributes

-- TODO: Prevent FOUC
type Config r i
  = { discrete :: Boolean
    , displayMarkers :: Boolean
    , min :: Maybe Float
    , max :: Maybe Float
    , step :: Maybe Float
    , value :: Maybe Float
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onInput :: Maybe (Float -> r i)
    }

defaultConfig :: Config r i
defaultConfig =
  { discrete: false
  , displayMarkers: false
  , min: Nothing
  , max: Nothing
  , step: Nothing
  , value: Nothing
  , disabled: false
  , additionalAttributes: []
  , onInput: Nothing
  }

{-| Specify whether a slider is _discrete_

Discrete sliders feature a pin that indicates the current value while
interacting with the slider.

This works best for integer-valued sliders, but this is not a requirement.

-}
{-| Specify whether a slider should display markers

Note that this option is ignored by non-discrete sliders.

-}
slider :: Config r i -> HH.HTML w i
slider (config_@{ additionalAttributes }) =
  HH.element "mdc-slider"
    ( Array.filterMap identity
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
        <> additionalAttributes
    )
    [ trackContainerElt
    , thumbContainerElt config_
    ]

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_slider)

displayCss :: Maybe (IProp r i)
displayCss = Just (style "display" "block")

discreteCs :: Config r i -> Maybe (IProp r i)
discreteCs { discrete } =
  if discrete then
    Just (HP.class_ mdc_slider____discrete)
  else
    Nothing

displayMarkersCs :: Config r i -> Maybe (IProp r i)
displayMarkersCs { discrete, displayMarkers } =
  if discrete && displayMarkers then
    Just (HP.class_ mdc_slider____display_markers)
  else
    Nothing

tabIndexProp :: Maybe (IProp r i)
tabIndexProp = Just (HH.Attributes.tabindex 0)

sliderRoleAttr :: Maybe (IProp r i)
sliderRoleAttr = Just (HH.Attributes.attribute "role" "slider")

valueProp :: Config r i -> Maybe (IProp r i)
valueProp { value } = map (HP.prop "value" << Encode.float) value

minProp :: Config r i -> Maybe (IProp r i)
minProp { min } = map (HP.prop "min" << Encode.float) min

maxProp :: Config r i -> Maybe (IProp r i)
maxProp { max } = map (HP.prop "max" << Encode.float) max

stepProp :: Config r i -> Maybe (IProp r i)
stepProp { step } = map (HP.prop "step" << Encode.float) step

disabledProp :: Config r i -> Maybe (IProp r i)
disabledProp { disabled } = Just (HP.prop "disabled" disabled)

ariaValueMinAttr :: Config r i -> Maybe (IProp r i)
ariaValueMinAttr { min } = map (HH.Attributes.attribute "aria-valuemin" << String.fromFloat) min

ariaValueMaxAttr :: Config r i -> Maybe (IProp r i)
ariaValueMaxAttr { max } = map (HH.Attributes.attribute "aria-valuemax" << String.fromFloat) max

ariaValuenowAttr :: Config r i -> Maybe (IProp r i)
ariaValuenowAttr { value } = map (HH.Attributes.attribute "aria-valuenow" << String.fromFloat) value

changeHandler :: Config r i -> Maybe (IProp r i)
changeHandler { onInput } =
  map
    ( \handler ->
        HH.Events.on "MDCSlider:input"
          (Decode.map handler (Decode.at [ "target", "value" ] Decode.float))
    )
    onInput

trackContainerElt :: HH.HTML w i
trackContainerElt = HH.div [ HP.class_ mdc_slider__track_container ] [ trackElt, trackMarkerContainerElt ]

trackElt :: HH.HTML w i
trackElt = HH.div [ HP.class_ mdc_slider__track ] []

trackMarkerContainerElt :: HH.HTML w i
trackMarkerContainerElt = HH.div [ HP.class_ mdc_slider__track_marker_container ] []

thumbContainerElt :: Config r i -> HH.HTML w i
thumbContainerElt { discrete } =
  HH.div [ HP.class_ mdc_slider__thumb_container ]
    ( if discrete then
        [ pinElt, thumbElt, focusRingElt ]
      else
        [ thumbElt, focusRingElt ]
    )

pinElt :: HH.HTML w i
pinElt = HH.div [ HP.class_ mdc_slider__pin ] [ pinValueMarkerElt ]

pinValueMarkerElt :: HH.HTML w i
pinValueMarkerElt = HH.div [ HP.class_ mdc_slider__pin_value_marker ] []

thumbElt :: HH.HTML w i
thumbElt =
  Halogen.SVG.Elements.svg
    [ Halogen.SVG.Attributes.class_ "mdc-slider__thumb"
    , Halogen.SVG.Attributes.width "21"
    , Halogen.SVG.Attributes.height "21"
    ]
    [ Halogen.SVG.Elements.circle
        [ Halogen.SVG.Attributes.cx "10.5"
        , Halogen.SVG.Attributes.cy "10.5"
        , Halogen.SVG.Attributes.r "7.875"
        ]
        []
    ]

focusRingElt :: HH.HTML w i
focusRingElt = HH.div [ HP.class_ mdc_slider__focus_ring ] []
