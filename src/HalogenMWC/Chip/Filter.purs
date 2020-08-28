module HalogenMWC.Chip.Filter where

import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config i =
  { icon :: Maybe String
  , selected :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , onChange :: Maybe (Event -> i)
  }

data Chip i = Chip (Config i) String

defaultConfig :: forall i . Config i
defaultConfig =
  { selected: false
  , icon: Nothing
  , onChange: Nothing
  , additionalAttributes: []
  }
