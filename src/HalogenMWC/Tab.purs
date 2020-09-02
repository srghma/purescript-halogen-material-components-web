module HalogenMWC.Tab where

import Data.Maybe (Maybe(..))
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

defaultConfig :: forall i. Content -> Config i
defaultConfig content =
  { active: false
  , additionalAttributes: []
  , onClick: Nothing
  , content
  }
