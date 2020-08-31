module Demo.Pages.Buttons where

import Demo.HOC.CatalogPage
import Demo.Utils
import Halogen
import Material.Classes.Typography
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Demo.Pages.Buttons.Css as Demo.Pages.Buttons.Css
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as Web.HTML.HTMLElement

type State = Unit

type Query = Const Void

data Action = Focus String

type Input = Unit

type Message = Void

catalogPage :: CatalogPage
catalogPage =
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

        render =
          HH.div_
            [ Button.button Button.Text       config [ HH.text "Text" ]
            , Button.button Button.Raised     config [ HH.text "Raised" ]
            , Button.button Button.Unelevated config [ HH.text "Unelevated" ]
            , Button.button Button.Outlined   config [ HH.text "Outlined" ]
            ]
      in
        H.mkComponent
          { initialState: const unit
          , render: const render
          , eval: H.mkEval H.defaultEval
          }
  , content:
      let
        render =
          HH.div_
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

        handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
        handleAction = case _ of
          Focus id -> H.liftEffect $ do
            (htmlElement :: HTMLElement) <- selectElement (QuerySelector ("#" <> id)) >>= maybe (throwError $ error $  "cannot find required element by id: " <> id) pure
            Web.HTML.HTMLElement.focus htmlElement
       in
      H.mkComponent
        { initialState: const unit
        , render: const render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }
  }

textButtons :: forall r w i . HH.HTML w i
textButtons = buttonsRow (Button.button Button.Text) []

raisedButtons :: forall r w i . HH.HTML w i
raisedButtons = buttonsRow (Button.button Button.Raised) []

unelevatedButtons :: forall r w i . HH.HTML w i
unelevatedButtons = buttonsRow (Button.button Button.Unelevated) []

outlinedButtons :: forall r w i . HH.HTML w i
outlinedButtons = buttonsRow (Button.button Button.Outlined) []

shapedButtons :: forall r w i . HH.HTML w i
shapedButtons = buttonsRow (Button.button Button.Unelevated) [ HP.style "border-radius: 18px;" ]

linkButtons :: forall r w i . HH.HTML w i
linkButtons = buttonsRow (\config -> Button.buttonLink Button.Text (config { additionalAttributes = [ HP.href "#buttons" ] })) []

focusButton :: forall r w i . HH.HTML w Action
focusButton =
  HH.div_
    [ Button.button Button.Raised (Button.defaultConfig { additionalAttributes = [ HP.id_ "my-button" ] }) [ HH.text "Button" ]
    , HH.text "\x00A0"
    , Button.button Button.Raised (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-button") ] }) [ HH.text "Focus" ]
    , HH.text "\x00A0"
    , Button.buttonLink Button.Raised (Button.defaultConfig { additionalAttributes = [ HP.id_ "my-link-button", HP.href "#buttons" ] }) [ HH.text "Link button" ]
    , HH.text "\x00A0"
    , Button.button Button.Raised (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-link-button") ] }) [ HH.text "Focus" ]
    ]

buttonsRow :: forall r w i . (Button.Config ( style :: String | r ) i -> Array (HH.HTML w i) -> HH.HTML w i) -> Array (IProp ( style :: String | r ) i) -> HH.HTML w i
buttonsRow button additionalAttributes =
  let
    config = Button.defaultConfig { additionalAttributes = [ rowMargin ] <> additionalAttributes }
  in
  HH.div_
    [ button config [ HH.text "Default" ]
    , button (config { additionalClasses = [ Demo.Pages.Buttons.Css.styles.my_button_dense ] }) [ HH.text "Dense" ]
    , button config [ Button.buttonIcon "favorite", HH.text "Icon" ]
    ]

heroMargin :: forall r w i . IProp ( style :: String | r ) i
heroMargin = HP.style "margin: 16px 32px;"

rowMargin :: forall r w i . IProp ( style :: String | r ) i
rowMargin = HP.style "margin: 8px 16px;"
