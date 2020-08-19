module HalogenMWC.Chip.Action where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { icon :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    }

data Chip r i
  = Chip (Config r i) String

defaultConfig :: Config r i
defaultConfig =
  { icon: Nothing
  , additionalAttributes: []
  , onClick: Nothing
  }
