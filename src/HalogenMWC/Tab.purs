module HalogenMWC.Tab where

import Protolude (Maybe(..))
import Web.Event.Event (Event)
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)

type Config i
  = { active :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLbutton i)
    , onClick :: Maybe (Event -> i)
    , content :: Content
    }

type Content
  = { label :: String
    , icon :: Maybe String
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { active: false
  , additionalAttributes: []
  , onClick: Nothing
  , content:
    { label: ""
    , icon: Nothing
    }
  }
