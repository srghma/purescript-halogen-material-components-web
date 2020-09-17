module Demo.Pages.Button where

import Protolude

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.MkComponent.WithFocus as WithFocus
import Demo.Pages.Button.Css as Demo.Pages.Button.Css
import Demo.Utils (mkComponentStatic)
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.Button.Link as Button.Link
import Material.Classes.Typography (mdc_typography____subtitle1)

type State = Unit

type Query = Const Void

type Input = Unit

type Message = Void

config :: CatalogPage
config =
  { title: "Button"
  , prelude: "Buttons communicate an action a user can take. They are typically placed throughout your UI, in places like dialogs, forms, cards, and toolbars."
  , resources:
    { materialDesignGuidelines: Just "https://material.io/go/design-buttons"
    , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Button"
    , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-button"
    }
  , hero:
    let
      config = Button.defaultConfig { additionalClasses = [ Demo.Pages.Button.Css.styles.heroMargin ] }

      render =
        HH.div_
          [ HH.slot_ (SProxy :: SProxy "text-button") unit Button.button ({ variant: Button.Text, config, content: [ HH.text "Text" ] })
          , Button.buttonView Button.Raised config [ HH.text "Raised" ]
          , Button.buttonView Button.Unelevated config [ HH.text "Unelevated" ]
          , Button.buttonView Button.Outlined config [ HH.text "Outlined" ]
          ]
    in
      mkComponentStatic render
  , content:
    let
      render :: forall w. HH.HTML w WithFocus.Action
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
    in
      WithFocus.mkComponent render
  }

textButtons :: forall w i. HH.HTML w i
textButtons = buttonsRow (Button.buttonView Button.Text) []

raisedButtons :: forall w i. HH.HTML w i
raisedButtons = buttonsRow (Button.buttonView Button.Raised) []

unelevatedButtons :: forall w i. HH.HTML w i
unelevatedButtons = buttonsRow (Button.buttonView Button.Unelevated) []

outlinedButtons :: forall w i. HH.HTML w i
outlinedButtons = buttonsRow (Button.buttonView Button.Outlined) []

shapedButtons :: forall w i. HH.HTML w i
shapedButtons = buttonsRow (Button.buttonView Button.Unelevated) [ Demo.Pages.Button.Css.styles.shapedButtons ]

linkButtons :: forall w i. HH.HTML w i
linkButtons =
  let
    additionalAttributes = [ HP.href "#buttons" ]

    config = Button.Link.defaultConfig { additionalAttributes = additionalAttributes, additionalClasses = [ Demo.Pages.Button.Css.styles.rowMargin ] }

    denseConf = Button.Link.defaultConfig { additionalAttributes = additionalAttributes, additionalClasses = [ Demo.Pages.Button.Css.styles.my_button_dense, Demo.Pages.Button.Css.styles.rowMargin ] }
  in
    HH.div_
      [ Button.Link.buttonLink Button.Link.Text config [ HH.text "Default" ]
      , Button.Link.buttonLink Button.Link.Text denseConf [ HH.text "Dense" ]
      , Button.Link.buttonLink Button.Link.Text config [ Button.buttonIconMaterialIcons "favorite", HH.text "Icon" ]
      ]


focusButton :: forall w. HH.HTML w WithFocus.Action
focusButton =
  HH.div_
    [ Button.buttonView Button.Raised (Button.defaultConfig { additionalAttributes = [ HP.id_ "my-button" ] }) [ HH.text "Button" ]
    , HH.text "\x00A0"
    , Button.buttonView Button.Raised (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ WithFocus.Focus "my-button") ] }) [ HH.text "Focus" ]
    , HH.text "\x00A0"
    , Button.Link.buttonLink Button.Raised (Button.Link.defaultConfig { additionalAttributes = [ HP.id_ "my-link-button", HP.href "#buttons" ] }) [ HH.text "Link button" ]
    , HH.text "\x00A0"
    , Button.buttonView Button.Raised (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ WithFocus.Focus "my-link-button") ] }) [ HH.text "Focus" ]
    ]

buttonsRow :: forall r w i. (Button.Config i -> Array (HH.HTML w i) -> HH.HTML w i) -> Array ClassName -> HH.HTML w i
buttonsRow button additionalClasses =
  let
    config = Button.defaultConfig { additionalClasses = [ Demo.Pages.Button.Css.styles.rowMargin ] <> additionalClasses }
  in
    HH.div_
      [ button config [ HH.text "Default" ]
      , button (config { additionalClasses = config.additionalClasses <> [ Demo.Pages.Button.Css.styles.my_button_dense ] }) [ HH.text "Dense" ]
      , button config [ Button.buttonIconMaterialIcons "favorite", HH.text "Icon" ]
      ]
