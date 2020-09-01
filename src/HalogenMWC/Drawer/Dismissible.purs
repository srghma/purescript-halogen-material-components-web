module HalogenMWC.Drawer.Dismissible where

import Prelude
import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen (ClassName, ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.Drawer (mdc_drawer, mdc_drawer____dismissible)
import Web.Event.Event (Event, EventType(..))

type Config i
  = { open :: Boolean
    , additionalClasses :: Array ClassName
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClose :: Maybe (Event -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { open: false
  , additionalClasses: []
  , additionalAttributes: []
  , onClose: Nothing
  }

drawer :: forall w i. Config i -> Array (HH.HTML w i) -> HH.HTML w i
drawer config =
  HH.element (ElemName "mdc-drawer")
    ( [ HP.classes $ [ mdc_drawer, mdc_drawer____dismissible ] <> config.additionalClasses
      , HP.prop (PropName "open") config.open
      ]
        <> Array.catMaybes [ map (HE.handler (EventType "MDCDrawer:close")) config.onClose ]
        <> config.additionalAttributes
    )
