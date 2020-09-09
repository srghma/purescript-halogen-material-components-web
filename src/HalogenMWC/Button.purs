module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Implementation
  , module Insides
  ) where

import Protolude

import DOM.HTML.Indexed as I
import Halogen (ComponentSlot, ElemName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
import HalogenMWC.Button.Foundation (attachFoundation)
import HalogenMWC.Button.Foundation as Foundation
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation as Implementation
import HalogenMWC.Button.Insides as Insides

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
      , HP.ref (H.RefLabel "button")
      ]
  in
    \content ->
      Implementation.wrapTouch
        [ HH.button (commonProps <> config.additionalAttributes) (Implementation.commonHtml content)
        ]

type Query = Const Void

type Input w i = { variant :: Variant, config :: Config i, insides :: Array (HH.HTML w i) }

data Action
  = Initialize

type Message = Void

type ChildSlots = ()

type State w i = Input w i

button :: H.Component Query (Input (H.ComponentSlot ChildSlots Aff Action) Action) Message Aff
button =
  H.mkComponent
    { initialState: identity
    , render: \{ variant, config, insides } -> buttonView variant config insides
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        -- | , finalize = Just Finalize
        , handleAction = handleAction
        }
    }

handleAction :: Action -> H.HalogenM (State (H.ComponentSlot ChildSlots Aff Action) Action) Action ChildSlots Message Aff Unit
handleAction Initialize = H.getHTMLElementRef (H.RefLabel "button") >>= traverse_ \htmlElement -> H.liftEffect $ void $ Foundation.attachFoundation htmlElement
