module Demo.Pages.Buttons where

import Demo.HOC.CatalogPage
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

data State = Unit

initialState :: forall r w i . State
initialState = unit

data Action = Focus String

render :: forall r w i . State -> CatalogPage Action
render model =
    { title: "Button"
    , prelude: "Buttons communicate an action a user can take. They are typically placed throughout your UI, in places like dialogs, forms, cards, and toolbars."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-buttons"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Button"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-button"
        }
    , hero: heroButtons
    , content:
        [ HH.h3 [ mdc_typography____subtitle1 ] [ HH.text "Text Button" ]
        , textButtons
        , HH.h3 [ mdc_typography____subtitle1 ] [ HH.text "Raised Button" ]
        , raisedButtons
        , HH.h3 [ mdc_typography____subtitle1 ] [ HH.text "Unelevated Button" ]
        , unelevatedButtons
        , HH.h3 [ mdc_typography____subtitle1 ] [ HH.text "Outlined Button" ]
        , outlinedButtons
        , HH.h3 [ mdc_typography____subtitle1 ] [ HH.text "Shaped Button" ]
        , shapedButtons
        , HH.h3 [ mdc_typography____subtitle1 ] [ HH.text "Link Button" ]
        , linkButtons
        , HH.h3 [ mdc_typography____subtitle1 ] [ HH.text "Focus Button" ]
        , focusButton
        ]
    }

heroButtons :: forall r w i . Array (HH.HTML w i)
heroButtons =
  let
    config = Button.defaultConfig { additionalAttributes = [ heroMargin ] }
  in
    [ Button.text config "Text"
    , Button.raised config "Raised"
    , Button.unelevated config "Unelevated"
    , Button.outlined config "Outlined"
    ]

textButtons :: forall r w i . HH.HTML w i
textButtons =
    buttonsRow Button.text []

raisedButtons :: forall r w i . HH.HTML w i
raisedButtons =
    buttonsRow Button.raised []

unelevatedButtons :: forall r w i . HH.HTML w i
unelevatedButtons =
    buttonsRow Button.unelevated []

outlinedButtons :: forall r w i . HH.HTML w i
outlinedButtons =
    buttonsRow Button.outlined []

shapedButtons :: forall r w i . HH.HTML w i
shapedButtons =
    buttonsRow Button.unelevated [ HP.style "border-radius: 18px;" ]

linkButtons :: forall r w i . HH.HTML w i
linkButtons =
    buttonsRow
        (\config label -> Button.text (config { href = (Just $ "#" <> label) }))
        []

focusButton :: forall r w i . HH.HTML Action
focusButton =
    HH.div []
        [ Button.raised
            (Button.defaultConfig
                { additionalAttributes = [ HP.id_ "my-button" ]
                }
            )
            "Button"
        , HH.text "\x00A0"
        , Button.raised (Button.defaultConfig { onClick = Focus "my-button" }) "Focus"
        , HH.text "\x00A0"
        , Button.raised
            (Button.defaultConfig
                { href = (Just "#")
                , additionalAttributes = [ HP.id_ "my-link-button" ]
                }
            )
            "Link button"
        , HH.text "\x00A0"
        , Button.raised (Button.defaultConfig { onClick = Focus "my-link-button" }) "Focus"
        ]

buttonsRow :: forall r w i . (Button.Config w i -> String -> HH.HTML w i) -> Array (IProp r i) -> HH.HTML w i
buttonsRow button additionalClasses =
    let
        config = Button.defaultConfig { additionalAttributes = [ rowMargin ] <> additionalAttributes }
    in
    HH.div []
        [ button config "Default"
        , button (config { dense = true }) "Dense"
        , button (config { icon = Just "favorite" }) "Icon"
        ]

heroMargin :: forall r w i . IProp r i
heroMargin =
    HP.style "margin: 16px 32px;"

rowMargin :: forall r w i . IProp r i
rowMargin =
    HP.style "margin: 8px 16px;"
