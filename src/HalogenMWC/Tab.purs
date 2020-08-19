module HalogenMWC.Tab where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML IProp
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Tab

type Config r i
  = { active :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    , content :: Content
    }

type Content
  = { label :: String
    , icon :: Maybe String
    }

newtype Tab r i
  = Tab (Config r i)

defaultConfig :: Config r i
defaultConfig =
  { active: false
  , additionalAttributes: []
  , onClick: Nothing
  , content:
    { label: ""
    , icon: Nothing
    }
  }

tab :: Config r i -> Content -> Tab r i
tab config_ content = Tab (config_ { content = content })
