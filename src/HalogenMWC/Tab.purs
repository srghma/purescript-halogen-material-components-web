module HalogenMWC.Tab where

import Halogen
import Material.Classes.Tab
import Protolude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Web.Event.Event (Event)

type Config i =
  { active :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLbutton i)
  , onClick :: Maybe (Event -> i)
  , content :: Content
  }

type Content =
  { label :: String
  , icon :: Maybe String
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { active: false
  , additionalAttributes: []
  , onClick: Nothing
  , content:
    { label: ""
    , icon: Nothing
    }
  }
