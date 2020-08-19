module HalogenMWC.Chip.Choice where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { icon :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    }

data Chip a r i
  = Chip (Config r i) a

defaultConfig :: Config r i
defaultConfig =
  { icon: Nothing
  , additionalAttributes: []
  }
