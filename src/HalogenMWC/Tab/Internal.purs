module HalogenMWC.Tab.Internal
    ( Config(..)
    , Content
    , Tab(..)
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
    =
        { active :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        , content :: Content
        }

data Content =
    { label :: String
    , icon :: Maybe String
    }

data Tab r i
    = Tab (Config r i)
