module HalogenMWC.Chip.Input where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { leadingIcon :: Maybe String
    , trailingIcon :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    , onDelete :: Maybe r i
    }

data Chip r i
  = Chip (Config r i) String

defaultConfig :: Config r i
defaultConfig =
  { leadingIcon: Nothing
  , trailingIcon: Nothing
  , additionalAttributes: []
  , onDelete: Nothing
  , onClick: Nothing
  }
