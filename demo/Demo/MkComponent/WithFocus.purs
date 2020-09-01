module Demo.MkComponent.WithFocus where

import Protolude (Aff, Unit, const, unit, ($))
import Halogen as H
import Halogen.HTML as HH
import Demo.Utils (focusById)

data Action
  = Focus String

handleAction :: forall state slots message. Action -> H.HalogenM state Action slots message Aff Unit
handleAction (Focus id) = H.liftEffect $ focusById id

mkComponent :: forall query input output slots m. HH.ComponentHTML Action slots Aff -> H.Component query input output Aff
mkComponent render =
  H.mkComponent
    { initialState: const unit
    , render: const render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
