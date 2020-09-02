module Demo.Pages.Switch where

import Demo.HOC.CatalogPage (CatalogPage)
import Data.Map (Map)
import Data.Map as Map
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
import HalogenMWC.FormField as FormField
import HalogenMWC.Switch as Switch
import Material.Classes.Typography
import Demo.Utils

type State = Map String Boolean

type ChildSlots = ()

type Message = Void

data Action
  = Toggle String
  | Focus String

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       Toggle id -> H.modify_ $ Map.alter (
        case _ of
             Nothing -> Just true
             Just v -> Just $ not v
       ) id
       Focus id -> H.liftEffect $ focusById id

isChecked :: forall r w i . String -> State -> Boolean
isChecked id state = Maybe.fromMaybe false (Map.lookup id state)

catalogPage :: CatalogPage
catalogPage =
  { title: "Switch"
  , prelude: "Switches communicate an action a user can take. They are typically placed throughout your UI, in places like dialogs, forms, cards, and toolbars."
  , resources:
      { materialDesignGuidelines: Just "https://material.io/go/design-switches"
      , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Switch"
      , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-switch"
      }
  , hero:
      H.mkComponent
        { initialState: const $ Map.fromFoldable [ ( "hero-switch" /\ true ) ]
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        , render: \state ->
            let
              id = "hero-switch"
            in
            HH.div_
            [ FormField.formField
                (FormField.defaultConfig
                    { label = "off/on"
                    , for = Just id
                    , onClick = Just $ const $ Toggle id
                    }
                )
                [ Switch.switch
                    (Switch.defaultConfig
                        { checked = isChecked id state
                        , onChange = Just $ const $ Toggle id
                        }
                    )
                ]
            ]
        }
  , content:
      H.mkComponent
        { initialState: const $ Map.empty
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        , render: \state ->
            HH.div_
            [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Switch" ]
            , demoSwitch state
            , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Switch" ]
            , focusSwitch state
            ]
        }
  }

demoSwitch :: forall r w i . State -> HH.HTML w Action
demoSwitch state =
  let
    id = "demo-switch"
  in
  FormField.formField
    ( FormField.defaultConfig
      { label = "off/on"
      , for = Just id
      , onClick = Just $ const $ Toggle id
      }
    )
    [ Switch.switch
        ( Switch.defaultConfig
          { checked = (isChecked id state)
          , onChange = Just $ const $ Toggle id
          }
        )
    ]

focusSwitch :: forall r w i . State -> HH.HTML w Action
focusSwitch state =
  let
    id = "my-switch"
  in
  HH.div_
      [ FormField.formField
          (FormField.defaultConfig
              { label = "off/on"
              , for = Just id
              , onClick = Just $ const $ Toggle id
              , additionalAttributes = [ HP.id_ "my-form-field" ]
              }
          )
          [ Switch.switch
              (Switch.defaultConfig
                  { checked = isChecked id state
                  , onChange = Just $ const $ Toggle id
                  , additionalAttributes = [ HP.id_ id ]
                  }
              )
          ]
      , HH.text "\x00A0"
      , Button.button Button.Raised
          (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-switch") ] })
          [ HH.text "Focus switch" ]
      , Button.button Button.Raised
          (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-form-field") ] })
          [ HH.text "Focus form field" ]
      ]
