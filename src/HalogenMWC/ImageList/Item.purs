module HalogenMWC.ImageList.Item where

import Protolude
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)

type Config i
  = { label :: Maybe String
    , href :: Maybe String
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , image :: String
    }

newtype ImageListItem w i
  = ImageListItem (Config i)

defaultConfig :: forall r i. Config i
defaultConfig =
  { label: Nothing
  , href: Nothing
  , additionalAttributes: []
  , image: ""
  }
