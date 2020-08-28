module HalogenMWC.Chip.Action where

import Halogen
import MaterialIconsFont.Classes
import Protolude
import Web.Event.Event

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config i =
  { icon :: Maybe String
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , onClick :: Maybe (Event -> i)
  }

data Chip i = Chip (Config i) String

defaultConfig :: forall i . Config i
defaultConfig =
  { icon: Nothing
  , additionalAttributes: []
  , onClick: Nothing
  }
