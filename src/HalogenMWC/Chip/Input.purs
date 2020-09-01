module HalogenMWC.Chip.Input where

import Data.Maybe (Maybe(..))
import Web.Event.Event (Event)
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i
  = { leadingIcon :: Maybe String
    , trailingIcon :: Maybe String
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClick :: Maybe (MouseEvent -> i)
    , onDelete :: Maybe (Event -> i)
    }

data Chip i = Chip (Config i) String

defaultConfig :: forall i. Config i
defaultConfig =
  { leadingIcon: Nothing
  , trailingIcon: Nothing
  , additionalAttributes: []
  , onDelete: Nothing
  , onClick: Nothing
  }
