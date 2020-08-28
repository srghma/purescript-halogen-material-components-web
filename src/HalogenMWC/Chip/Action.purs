module HalogenMWC.Chip.Action where

import Prelude
import Web.Event.Event (Event)
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)

type Config i
  = { icon :: Maybe String
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClick :: Maybe (Event -> i)
    }

data Chip i
  = Chip (Config i) String

defaultConfig :: forall i. Config i
defaultConfig =
  { icon: Nothing
  , additionalAttributes: []
  , onClick: Nothing
  }
