module HalogenMWC.ImageList.Item where

import Protolude
import Halogen.HTML (IProp)

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
