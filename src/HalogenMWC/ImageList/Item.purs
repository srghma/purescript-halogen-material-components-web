module HalogenMWC.ImageList.Item where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { label :: Maybe String
    , href :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , image :: String
    }

newtype ImageArrayItem r i
  = ImageArrayItem (Config r i)

defaultConfig :: forall r i . Config r i
defaultConfig =
  { label: Nothing
  , href: Nothing
  , additionalAttributes: []
  , image: ""
  }
