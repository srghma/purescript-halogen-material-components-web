module Demo.Pages.Buttons where

-- | import Demo.HOC.CatalogPage
import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import Material.Classes.Typography

type State
  = Unit

type Query
  = Const Void

data Action
  = Focus String

type Input
  = Unit

type Message
  = Void

type CatalogPageResources =
  { materialDesignGuidelines :: Maybe String
  , documentation :: Maybe String
  , sourceCode :: Maybe String
  }

type CatalogPage w i =
  { title :: String
  , prelude :: String
  , resources :: CatalogPageResources
  , hero :: Array (HH.HTML w i)
  , content :: Array (HH.HTML w i)
  }

component ::
  ∀ m. H.Component Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render: \state -> HH.div_ render.content
    , eval: H.mkEval H.defaultEval
    }

render :: forall w . CatalogPage w Action
render =
  { title: "Button"
  , prelude: "Buttons communicate an action a user can take. They are typically placed throughout your UI, in places like dialogs, forms, cards, and toolbars."
  , resources:
      { materialDesignGuidelines: Just "https://material.io/go/design-buttons"
      , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Button"
      , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-button"
      }
  , hero:
    let
      config = Button.defaultConfig { additionalAttributes = [ heroMargin ] }
    in
      [ Button.button Button.Text       config [ HH.text "Text" ]
      , Button.button Button.Raised     config [ HH.text "Raised" ]
      , Button.button Button.Unelevated config [ HH.text "Unelevated" ]
      , Button.button Button.Outlined   config [ HH.text "Outlined" ]
      ]
  , content:
      [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Text Button" ]
      , textButtons
      , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Raised Button" ]
      , raisedButtons
      , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Unelevated Button" ]
      , unelevatedButtons
      , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Outlined Button" ]
      , outlinedButtons
      , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Shaped Button" ]
      , shapedButtons
      , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Link Button" ]
      , linkButtons
      , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Button" ]
      , focusButton
      ]
  }

textButtons :: forall r w i . HH.HTML w i
textButtons =
    buttonsRow (Button.button Button.Text) []

raisedButtons :: forall r w i . HH.HTML w i
raisedButtons =
    buttonsRow (Button.button Button.Raised) []

unelevatedButtons :: forall r w i . HH.HTML w i
unelevatedButtons =
    buttonsRow (Button.button Button.Unelevated) []

outlinedButtons :: forall r w i . HH.HTML w i
outlinedButtons =
    buttonsRow (Button.button Button.Outlined) []

shapedButtons :: forall r w i . HH.HTML w i
shapedButtons =
    buttonsRow (Button.button Button.Unelevated) [ HP.style "border-radius: 18px;" ]

linkButtons :: forall r w i . HH.HTML w i
linkButtons =
    buttonsRow
        (\config -> Button.buttonLink Button.Text (config { additionalAttributes = [ HP.href "#buttons" ] }))
        []

focusButton :: forall r w i . HH.HTML w Action
focusButton =
    HH.div []
        [ Button.button Button.Raised
            (Button.defaultConfig
                { additionalAttributes = [ HP.id_ "my-button" ]
                }
            )
            [ HH.text "Button" ]
        , HH.text "\x00A0"
        , Button.button Button.Raised
          (Button.defaultConfig { additionalAttributes = [ HE.onClick (\_ -> Focus "my-button") ] })
          [ HH.text "Focus" ]
        , HH.text "\x00A0"
        , Button.buttonLink Button.Raised
            (Button.defaultConfig
                { additionalAttributes = [ HP.id_ "my-link-button", HP.href "#buttons" ]
                }
            )
            [ HH.text "Link button" ]
        , HH.text "\x00A0"
        , Button.button Button.Raised
          (Button.defaultConfig { additionalAttributes = [ HE.onClick (\_ -> Focus "my-link-button") ] })
          [ HH.text "Focus" ]
        ]

buttonsRow :: forall r w i . (Button.Config ( style :: String | r ) i -> Array (HH.HTML w i) -> HH.HTML w i) -> Array (IProp ( style :: String | r ) i) -> HH.HTML w i
buttonsRow button additionalAttributes =
    let
      config = Button.defaultConfig { additionalAttributes = [ rowMargin ] <> additionalAttributes }
    in
    HH.div []
        [ button config [ HH.text "Default" ]
        -- | , button (config { additionalClasses = [ mdc_button___dense ] }) [ HH.text "Dense" ]
        , button config [ Button.buttonIcon "favorite", HH.text "Icon" ]
        ]

heroMargin :: forall r w i . IProp ( style :: String | r ) i
heroMargin =
    HP.style "margin: 16px 32px;"

rowMargin :: forall r w i . IProp ( style :: String | r ) i
rowMargin =
    HP.style "margin: 8px 16px;"
