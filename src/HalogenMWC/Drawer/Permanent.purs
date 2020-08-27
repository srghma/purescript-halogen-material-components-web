module HalogenMWC.Drawer.Permanent where

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

drawer :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
drawer = HH.div [ HP.class_ mdc_drawer ]

content :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
content = HH.div [ HP.class_ mdc_drawer__content ]

header :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
header = HH.div [ HP.class_ mdc_drawer__header ]
