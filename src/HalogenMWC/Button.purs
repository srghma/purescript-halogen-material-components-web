module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Implementation
  , module Insides
  ) where

import HalogenMWC.Ripple.Bounded
import Protolude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.String as String
import FRP.Event as Event
import Halogen (ComponentSlot, ElemName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events (onKeyUp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation as Implementation
import HalogenMWC.Button.Insides as Insides
import HalogenMWC.Utils as Utils
import Web.DOM (Element)
import Web.DOM.Document as Web.DOM.Document
import Web.DOM.Element as Web.DOM.Element
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event.Event
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.Window (Window)
import Web.HTML.Window as Web.HTML.Window
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import HalogenMWC.Ripple.Common as Ripple
import HalogenMWC.Ripple.Bounded as Ripple
import HalogenMWC.Ripple.Constants as Ripple
import HalogenMWC.Ripple.HTML as Ripple

type Config i =
  { disabled :: Boolean
  , additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLbutton i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { disabled: false
  , additionalClasses: []
  , additionalAttributes: []
  }

buttonView :: forall w i. Variant -> Config i -> Array (HH.HTML w i) -> HH.HTML w i
buttonView variant config =
  let
    commonProps =
      [ HP.classes (Implementation.commonClasses variant <> config.additionalClasses)
      , HP.disabled config.disabled
      , HP.tabIndex (if config.disabled then -1 else 0)
      , HP.ref buttonRefLabel
      ]
  in
    \content ->
      Implementation.wrapTouch
        [ HH.button (commonProps <> config.additionalAttributes) (Implementation.commonHtml content)
        ]

type Query = Const Void

type Input w i =
  { variant :: Variant
  , config :: Config i
  , content :: Array (HH.HTML w i)
  }

type ChildSlots = ()

type State w i =
  { input       :: Input w i
  , rippleState :: Ripple.RippleState
  }

data Action = RippleAction Ripple.RippleAction__Common

type Message = Void

styleVar :: String -> String -> String
styleVar x y = x <> ": " <> y

buttonRefLabel :: H.RefLabel
buttonRefLabel = H.RefLabel "button"

isUnbounded = false

button :: H.Component Query (Input (H.ComponentSlot ChildSlots Aff Action) Action) Message Aff
button =
  H.mkComponent
    { initialState: \input ->
      { input
      , rippleState: Ripple.initialRippleState
      }
    , render: \state ->
        Implementation.wrapTouch
          [ HH.button
            ( [ HP.classes $
                ( Implementation.commonClasses state.input.variant
                <> Ripple.rippleClasses false state.rippleState
                <> state.input.config.additionalClasses
                )
              , HP.style $ Ripple.rippleStyles state.rippleState
              , HP.disabled state.input.config.disabled
              , HP.tabIndex (if state.input.config.disabled then -1 else 0)
              , HP.ref buttonRefLabel
              ]
            <> (Ripple.rippleProps <#> map RippleAction)
            <> state.input.config.additionalAttributes
            )
            (Implementation.commonHtml state.input.content)
          ]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
      }

handleAction :: Action -> H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       RippleAction rippleAction -> do
         state <- H.get

         Halogen.Query.HalogenM.imapState (\rippleState -> state { rippleState = rippleState }) (\state -> state.rippleState)
          $ Halogen.Query.HalogenM.mapAction RippleAction
          $ Ripple.handleAction state.input.config.disabled (H.getHTMLElementRef buttonRefLabel) rippleAction
