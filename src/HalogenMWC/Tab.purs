module HalogenMWC.Tab
    ( Config, config



    , Tab, tab, Content
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Tab.Internal (Config(..), Tab(..))



type Config r i =
    Material.Tab.Internal.Config r i



config :: Config r i
config =
    Config
        { active = False
        , additionalAttributes = []
        , onClick = Nothing
        , content = { label = "", icon = Nothing }
        }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



setActive :: Boolean -> Config r i -> Config r i
setActive active (Config config_) =
    Config { config_ | active = active }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



data Content =
    { label :: String
    , icon :: Maybe String
    }


{-| Tab type

Tabs can only be rendered within a [tab bar](Material-TabBar).

-}
data Tab r i =
    Material.Tab.Internal.Tab r i



tab :: Config r i -> Content -> Tab r i
tab (Config config_) content =
    Tab (Config { config_ | content = content })
