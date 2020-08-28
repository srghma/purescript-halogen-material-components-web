module HalogenMWC.Drawer.Permanent where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Drawer (mdc_drawer, mdc_drawer__content, mdc_drawer__header)

drawer :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
drawer = HH.div [ HP.class_ mdc_drawer ]

content :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
content = HH.div [ HP.class_ mdc_drawer__content ]

header :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
header = HH.div [ HP.class_ mdc_drawer__header ]
