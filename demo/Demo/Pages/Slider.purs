module Demo.Pages.Slider where

import Demo.HOC.CatalogPage (CatalogPage)
import Data.Map (Map)
import Data.Map as Map
import Protolude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import HalogenMWC.Button as Button
import HalogenMWC.Slider as Slider
import Material.Classes.Typography (mdc_typography____subtitle1)
import Demo.Utils (focusById)

type State = Map String Number

type ChildSlots = ()

type Message = Void

data Action
    = Changed String Number
    | Focus String

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
    case _ of
        Changed id value -> H.modify_ (Map.insert id value)
        Focus id -> H.liftEffect $ focusById id

config :: CatalogPage
config =
    { title: "Slider"
    , prelude: "Sliders let users select from a range of values by moving the slider thumb."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-sliders"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Slider"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-slider"
        }
    , hero:
      H.mkComponent
        { initialState: const $ Map.fromFoldable
            [ ( "hero-slider" /\ 25.0 )
            ]
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        , render: \state ->
            let
              id = "hero-slider"
            in
            Slider.slider
              (Slider.defaultConfig
                { value = Map.lookup id state
                , onInput = Just $ Changed id
                }
              )
        }
    , content:
        H.mkComponent
          { initialState: const $ Map.fromFoldable
            [ ( "continuous-slider" /\ 25.0 )
            , ( "discrete-slider" /\ 25.0 )
            , ( "discrete-slider-with-tick-marks" /\ 25.0 )
            ]
          , eval: H.mkEval H.defaultEval { handleAction = handleAction }
          , render: \state ->
              HH.div_
                [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Continuous" ]
                , continuousSlider state
                , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Discrete" ]
                , discreteSlider state
                , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Discrete with Tick Marks" ]
                , discreteSliderWithTickMarks state
                , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Slider" ]
                , focusSlider state
                ]
          }
    }

continuousSlider :: forall r w i . State -> HH.HTML w Action
continuousSlider state =
  let
    id = "continuous-slider"
  in
  Slider.slider
      (Slider.defaultConfig
          { value = Map.lookup id state
          , onInput = Just $ Changed id
          , min = Just 0.0
          , max = Just 50.0
          }
      )

discreteSlider :: forall r w i . State -> HH.HTML w Action
discreteSlider state =
  let
    id = "discrete-slider"
  in
  Slider.slider
      (Slider.defaultConfig
          { value = Map.lookup id state
          , onInput = Just $ Changed id
          , discrete = true
          , min = Just 0.0
          , max = Just 50.0
          , step = Just 1.0
          }
      )

discreteSliderWithTickMarks :: forall r w i . State -> HH.HTML w Action
discreteSliderWithTickMarks state =
  let
    id = "discrete-slider-with-tick-marks"
  in
  Slider.slider
    (Slider.defaultConfig
        { value = Map.lookup id state
        , onInput = Just $ Changed id
        , discrete = true
        , min = Just 0.0
        , max = Just 50.0
        , step = Just 1.0
        , displayMarkers = true
        }
    )

focusSlider :: forall r w i . State -> HH.HTML w Action
focusSlider state =
  let
    id = "my-slider"
  in
  HH.div_
    [ Slider.slider
        (Slider.defaultConfig
          { value = Map.lookup id state
          , onInput = Just $ Changed id
          , additionalAttributes = [ HP.id_ id ]
          }
        )
    , HH.text "\x00A0"
    , Button.buttonView Button.Raised
        (Button.defaultConfig { additionalAttributes = [ HE.onClick (\_ -> Focus id) ] })
        [ HH.text "Focus" ]
    ]
