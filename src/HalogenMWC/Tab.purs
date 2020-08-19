module HalogenMWC.Tab where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Tab (Config(..), Tab(..))

type Config r i =
    Material.Tab.Config r i

defaultConfig :: Config r i
defaultConfig =
        { active: False
        , additionalAttributes: []
        , onClick: Nothing
        , content: { label = "", icon = Nothing }
        }

data Content =
    { label :: String
    , icon :: Maybe String
    }

{-| Tab type

Tabs can only be rendered within a [tab bar](Material-TabBar).

-}
data Tab r i =
    Material.Tab.Tab r i

tab :: Config r i -> Content -> Tab r i
tab (Config config_) content =
    Tab { config_ { content = content }
