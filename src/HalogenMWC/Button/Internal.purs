module HalogenMWC.Button.Internal (Config(..)) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { icon :: Maybe String
    , trailingIcon :: Boolean
    , disabled :: Boolean
    , dense :: Boolean
    , href :: Maybe String
    , target :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    , touch :: Boolean
    }
