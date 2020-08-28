module HalogenMWC.Chip.Filter where

import Prelude
import DOM.HTML.Indexed as I
import Web.Event.Event (Event)
import Halogen.HTML (IProp)

type Config i
  = { icon :: Maybe String
    , selected :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onChange :: Maybe (Event -> i)
    }

data Chip i
  = Chip (Config i) String

defaultConfig :: forall i. Config i
defaultConfig =
  { selected: false
  , icon: Nothing
  , onChange: Nothing
  , additionalAttributes: []
  }
