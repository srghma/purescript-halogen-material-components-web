module HalogenMWC.Array.Item
  ( Config(..)
  , ArrayItem(..)
  , Selection(..)
  ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { disabled :: Boolean
    , selection :: Maybe Selection
    , href :: Maybe String
    , target :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    , node :: Html r i
    }

data Selection
  = Selected
  | Activated

data ArrayItem r i
  = ArrayItem (Config r i)
  | ArrayItemDivider (Html r i)
  | ArrayGroupSubheader (Html r i)
