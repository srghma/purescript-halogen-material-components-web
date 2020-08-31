module Demo.MkComponent.WithFocus where

import Protolude
import Web.HTML

import Halogen as H
import Halogen.HTML as HH
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (focus) as Web.HTML.HTMLElement
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Demo.Utils

data Action = Focus String

handleAction :: forall state slots message . Action -> H.HalogenM state Action slots message Aff Unit
handleAction (Focus id) = H.liftEffect $ focusById id

mkComponent :: forall query input output slots m . HH.ComponentHTML Action slots Aff -> H.Component query input output Aff
mkComponent render =
  H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }
