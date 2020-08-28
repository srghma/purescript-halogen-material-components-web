module HalogenMWC.Chip.Input where

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
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i =
  { leadingIcon :: Maybe String
  , trailingIcon :: Maybe String
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , onClick :: Maybe (MouseEvent -> i)
  , onDelete :: Maybe (Event -> i)
  }

data Chip i = Chip (Config i) String

defaultConfig :: forall i . Config i
defaultConfig =
  { leadingIcon: Nothing
  , trailingIcon: Nothing
  , additionalAttributes: []
  , onDelete: Nothing
  , onClick: Nothing
  }
