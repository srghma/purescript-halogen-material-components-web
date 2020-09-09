module Demo.Pages.RadioButton where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude (Aff, Maybe(..), Unit, Void, const, ($), (==))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import HalogenMWC.Button as Button
import HalogenMWC.FormField as FormField
import HalogenMWC.Radio as Radio
import Material.Classes.Typography (mdc_typography____subtitle1)
import Demo.Utils (focusById)

type State = String

type ChildSlots = ()

type Message = Void

data Action
    = Set String
    | Focus String

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
    case _ of
        Set index -> H.put index
        Focus id -> H.liftEffect $ focusById id

config :: CatalogPage
config =
  { title: "Radio Button"
  , prelude: "Buttons communicate an action a user can take. They are typically placed throughout your UI, in places like dialogs, forms, cards, and toolbars."
  , resources:
      { materialDesignGuidelines: Just "https://material.io/go/design-radio-buttons"
      , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Radio"
      , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-radio"
      }
  , hero:
    H.mkComponent
      { initialState: const "radio-buttons-hero-radio-1"
      , eval: H.mkEval H.defaultEval { handleAction = handleAction }
      , render: \state ->
          HH.div_
            [ heroRadio state "radio-buttons-hero-radio-1"
            , heroRadio state "radio-buttons-hero-radio-2"
            ]
      }
  , content:
    H.mkComponent
      { initialState: const "radio-buttons-example-radio-1"
      , eval: H.mkEval H.defaultEval { handleAction = handleAction }
      , render: \state ->
          HH.div_
            [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Radio Buttons" ]
            , HH.div_
                [ radio state "radio-buttons-example-radio-1" "Radio 1"
                , radio state "radio-buttons-example-radio-2" "Radio 2"
                ]
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Radio Button" ]
            , HH.div_
                [ Radio.radio (Radio.defaultConfig { additionalAttributes = [ HP.id_ "my-radio" ] })
                , HH.text "\x00A0"
                , Button.buttonView Button.Raised
                    (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-radio") ] })
                    [ HH.text "Focus" ]
                ]
            ]
      }
  }

heroRadio :: forall r w i . State -> String -> HH.HTML w Action
heroRadio state index =
  Radio.radio
    (Radio.defaultConfig
        { checked = index == state
        , onChange = Just $ const $ Set index
        , additionalAttributes = [ HP.style "margin: 0 10px;" ]
        }
    )

radio :: forall r w i . State -> String -> String -> HH.HTML w Action
radio state index label =
    FormField.formField
        (FormField.defaultConfig
            { label = label
            , for = Just index
            , onClick = Just $ const $ Set index
            , additionalAttributes = [ HP.style "margin: 0 10px;" ]
            }
        )
        [ Radio.radio
            (Radio.defaultConfig
                { checked = index == state
                , onChange = Just $ const $ Set index
                }
            )
        ]
