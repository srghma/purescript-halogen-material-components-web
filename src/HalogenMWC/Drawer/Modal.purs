module HalogenMWC.Drawer.Modal where

import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Drawer

type Config i =
  { open :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , onClose :: Maybe (Event -> i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { open: false
  , additionalAttributes: []
  , onClose: Nothing
  }

drawer :: forall w i . Config i -> Array (HH.HTML w i) -> HH.HTML w i
drawer config =
  HH.element (ElemName "mdc-drawer")
    ( [ HP.classes [ mdc_drawer, mdc_drawer____modal ]
      , HP.prop (PropName "open") config.open
      ]
      <> Array.catMaybes
        [ map (HE.handler (EventType "MDCDrawer:close")) config.onClose
        ]
      <> config.additionalAttributes
    )

content :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
content = HH.div [ HP.class_ mdc_drawer__content ]

header :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
header = HH.div [ HP.class_ mdc_drawer__header ]

scrim :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
scrim = HH.div [ HP.class_ mdc_drawer_scrim ]
