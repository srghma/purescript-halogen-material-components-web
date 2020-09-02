module Demo.Pages.Snackbar where

import Demo.Utils
import Halogen
import Material.Classes.Button
import Material.Classes.IconButton
import Material.Classes.Snackbar
import MaterialIconsFont.Classes
import Protolude

import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Maybe as Maybe
import Demo.HOC.CatalogPage (CatalogPage)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.Snackbar as Snackbar

type State = { queue :: Snackbar.Queue Action }

type ChildSlots = ()

type Message = Void

initialState :: forall r w i . State
initialState = { queue: Snackbar.initialQueue }

data Action
    = ShowBaseline
    | ShowLeading
    | ShowStacked
    | SnackbarClosed Snackbar.MessageId
    | Click Snackbar.MessageId

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       ShowBaseline             -> H.modify_ \state -> state { queue = Snackbar.addMessage baselineMessage state.queue }
       ShowLeading              -> H.modify_ \state -> state { queue = Snackbar.addMessage leadingMessage state.queue }
       ShowStacked              -> H.modify_ \state -> state { queue = Snackbar.addMessage stackedMessage state.queue }
       SnackbarClosed messageId -> H.modify_ \state -> state { queue = Snackbar.close messageId state.queue }
       Click messageId          -> pure unit

baselineMessage :: Snackbar.Message Action
baselineMessage =
  (Snackbar.defaultMessage "Can't send photo. Retry in 5 seconds.")
    { actionButton = Just "Retry"
    , onActionButtonClick = Just Click
    , actionIcon = Just "close"
    }

leadingMessage :: Snackbar.Message Action
leadingMessage =
  (Snackbar.defaultMessage "Your photo has been archived.")
    { leading = true
    , actionButton = Just "Undo"
    , onActionButtonClick = Just Click
    , actionIcon = Just "close"
    }

stackedMessage :: Snackbar.Message Action
stackedMessage =
  (Snackbar.defaultMessage "This item already has the label \"travel\". You can add a new label.")
    { stacked = true
    , actionButton = Just "Add a new label"
    , onActionButtonClick = Just Click
    , actionIcon = Just "close"
    }

catalogPage :: CatalogPage
catalogPage =
    { title: "Snackbar"
    , prelude: "Snackbars provide brief feedback about an operation through a message at the bottom of the screen."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-snackbar"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Snackbar"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-snackbar"
        }
    , hero: mkComponentStatic $
        HH.div
        [ HP.classes [ mdc_snackbar, mdc_snackbar____open ]
        , HP.style "position: relative; left: 0; transform: none;"
        ]
        [ HH.div
            [ HP.class_ mdc_snackbar__surface ]
            [ HH.div
                [ HP.class_ mdc_snackbar__label
                , HP.style "color: hsla(0,0%,100%,.87);"
                , HP.attr (AttrName "role") "status"
                , HP.attr (AttrName "aria-live") "polite"
                ]
                [ HH.text "Can't send photo. Retry in 5 seconds." ]
            , HH.div
                [ HP.class_ mdc_snackbar__actions ]
                [ HH.button
                    [ HP.classes [ mdc_button, mdc_snackbar__action ]
                    , HP.type_ ButtonButton
                    ]
                    [ HH.text "Retry" ]
                , HH.button
                    [ HP.classes [ mdc_icon_button, mdc_snackbar__dismiss, material_icons ]
                    ]
                    [ HH.text "close" ]
                ]
            ]
        ]
    , content:
        H.mkComponent
          { initialState: const initialState
          , eval: H.mkEval H.defaultEval { handleAction = handleAction }
          , render: \state ->
              HH.div_
              [ Button.button Button.Raised
                  (Button.defaultConfig
                      { additionalAttributes = [ buttonMargin, HE.onClick (const ShowBaseline) ]
                      }
                  )
                  [ HH.text "Baseline" ]
              , HH.text " "
              , Button.button Button.Raised
                  (Button.defaultConfig
                      { additionalAttributes = [ buttonMargin, HE.onClick (const ShowLeading) ]
                      }
                  )
                  [ HH.text "Leading" ]
              , HH.text " "
              , Button.button Button.Raised
                  (Button.defaultConfig
                      { additionalAttributes = [ buttonMargin, HE.onClick (const ShowStacked) ]
                      }
                  )
                  [ HH.text "Stacked" ]
              , Snackbar.snackbar
                  ((Snackbar.defaultConfig { onClosed: SnackbarClosed })
                      { closeOnEscape = true
                      }
                  )
                  state.queue
              ]
          }
    }

buttonMargin :: forall r i . IProp ( style :: String | r ) i
buttonMargin = HP.style "margin: 14px;"
