module HalogenMWC.Slider where

import Halogen (AttrName(..), ElemName(..), PropName(..))
import Material.Classes.Slider (mdc_slider, mdc_slider____discrete, mdc_slider____display_markers, mdc_slider__focus_ring, mdc_slider__pin, mdc_slider__pin_value_marker, mdc_slider__thumb, mdc_slider__thumb_container, mdc_slider__track, mdc_slider__track_container, mdc_slider__track_marker_container)
import Protolude (Maybe(..), map, show, ($), (&&), (<<<), (<>))
import Web.Event.Event (EventType(..))

import DOM.HTML.Indexed as I
import Data.Array as Array
import Foreign (readNumber)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.SVG.Attributes as Halogen.SVG.Attributes
import Halogen.SVG.Elements as Halogen.SVG.Elements
import HalogenMWC.Utils as Utils

-- TODO: Prevent FOUC
type Config i =
  { discrete :: Boolean
  , displayMarkers :: Boolean
  , min :: Maybe Number
  , max :: Maybe Number
  , step :: Maybe Number
  , value :: Maybe Number
  , disabled :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , onInput :: Maybe (Number -> i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { discrete:             false
  , displayMarkers:       false
  , min:                  Nothing
  , max:                  Nothing
  , step:                 Nothing
  , value:                Nothing
  , disabled:             false
  , additionalAttributes: []
  , onInput:              Nothing
  }

slider :: forall w i . Config i -> HH.HTML w i
slider config =
  HH.element (ElemName "mdc-slider")
    ( Array.catMaybes
        [ Just $ HP.classes $ Array.catMaybes
          [ Just mdc_slider
          , if config.discrete then Just mdc_slider____discrete else Nothing
          , if config.discrete && config.displayMarkers then Just mdc_slider____display_markers else Nothing
          ]
        , Just $ HP.attr (AttrName "style") "display: block;"
        , Just $ HP.tabIndex 0
        , Just $ HP.attr (AttrName "role") "slider"
        , Just $ HP.prop (PropName "disabled") config.disabled
        , map (HP.prop (PropName "value")) config.value
        , map (HP.prop (PropName "min")) config.min
        , map (HP.prop (PropName "max")) config.max
        , map (HP.prop (PropName "step")) config.step
        , map (HP.attr (AttrName "aria-valuemin") <<< show) config.min
        , map (HP.attr (AttrName "aria-valuemax") <<< show) config.max
        , map (HP.attr (AttrName "aria-valuenow") <<< show) config.value
        , changeHandler config.onInput
        ]
        <> config.additionalAttributes
    )
    [ trackContainerElt
    , thumbContainerElt config
    ]

changeHandler :: forall r i . Maybe (Number -> i) -> Maybe (IProp r i)
changeHandler = map (Utils.addForeignPropHandler (EventType "MDCSlider:input") "value" readNumber)

trackContainerElt :: forall w i . HH.HTML w i
trackContainerElt = HH.div [ HP.class_ mdc_slider__track_container ] [ trackElt, trackMarkerContainerElt ]

trackElt :: forall w i . HH.HTML w i
trackElt = HH.div [ HP.class_ mdc_slider__track ] []

trackMarkerContainerElt :: forall w i . HH.HTML w i
trackMarkerContainerElt = HH.div [ HP.class_ mdc_slider__track_marker_container ] []

thumbContainerElt :: forall w i . Config i -> HH.HTML w i
thumbContainerElt config =
  HH.div [ HP.class_ mdc_slider__thumb_container ]
    ( if config.discrete then
        [ pinElt, thumbElt, focusRingElt ]
      else
        [ thumbElt, focusRingElt ]
    )

pinElt :: forall w i . HH.HTML w i
pinElt = HH.div [ HP.class_ mdc_slider__pin ] [ pinValueMarkerElt ]

pinValueMarkerElt :: forall w i . HH.HTML w i
pinValueMarkerElt = HH.div [ HP.class_ mdc_slider__pin_value_marker ] []

thumbElt :: forall w i . HH.HTML w i
thumbElt =
  Halogen.SVG.Elements.svg
    [ Halogen.SVG.Attributes.class_ mdc_slider__thumb
    , Halogen.SVG.Attributes.width 21.0
    , Halogen.SVG.Attributes.height 21.0
    ]
    [ Halogen.SVG.Elements.circle
        [ Halogen.SVG.Attributes.cx 10.5
        , Halogen.SVG.Attributes.cy 10.5
        , Halogen.SVG.Attributes.r 7.875
        ]
        []
    ]

focusRingElt :: forall w i . HH.HTML w i
focusRingElt = HH.div [ HP.class_ mdc_slider__focus_ring ] []
