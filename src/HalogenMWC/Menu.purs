module HalogenMWC.Menu where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { open :: Boolean
    , quickOpen :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onClose :: Maybe r i
    }

defaultConfig :: Config r i
defaultConfig =
  { open: false
  , quickOpen: false
  , additionalAttributes: []
  , onClose: Nothing
  }

menu :: Config r i -> Array (HH.HTML w i) -> HH.HTML w i
menu (config_@{ additionalAttributes }) nodes =
  HH.element "mdc-menu"
    ( Array.catMaybes
        [ rootCs
        , openProp config_
        , quickOpenProp config_
        , closeHandler config_
        ]
        <> additionalAttributes
    )
    nodes

surfaceAnchor :: IProp r i
surfaceAnchor = HP.class_ mdc_menu_surface____anchor

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ "mdc-menu mdc-menu-surface")

openProp :: Config r i -> Maybe (IProp r i)
openProp { open } = Just (HP.prop "open" open)

quickOpenProp :: Config r i -> Maybe (IProp r i)
quickOpenProp { quickOpen } = Just (HP.prop "quickOpen" quickOpen)

closeHandler :: Config r i -> Maybe (IProp r i)
closeHandler { onClose } = map (HH.Events.on "MDCMenu:close" << Decode.succeed) onClose
