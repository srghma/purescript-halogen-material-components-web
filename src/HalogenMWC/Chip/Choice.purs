module HalogenMWC.Chip.Choice where

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
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

data Chip a i = Chip (Config i) a

defaultConfig :: forall i . Config i
defaultConfig =
  { icon: Nothing
  , additionalAttributes: []
  }
