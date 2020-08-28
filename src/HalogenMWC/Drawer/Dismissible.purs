module HalogenMWC.Drawer.Dismissible where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Web.Event.Event (Event, EventType(..))
import Data.Array as Array
import Halogen (ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.Drawer (mdc_drawer, mdc_drawer____dismissible)

type Config i
  = { open :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClose :: Maybe (Event -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { open: false
  , additionalAttributes: []
  , onClose: Nothing
  }

drawer :: forall w i. Config i -> Array (HH.HTML w i) -> HH.HTML w i
drawer config =
  HH.element (ElemName "mdc-drawer")
    ( [ HP.classes [ mdc_drawer, mdc_drawer____dismissible ]
      , HP.prop (PropName "open") config.open
      ]
        <> Array.catMaybes
            [ map (HE.handler (EventType "MDCDrawer:close")) config.onClose
            ]
        <> config.additionalAttributes
    )
