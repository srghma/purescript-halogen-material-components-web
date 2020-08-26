module HalogenMWC.LayoutGrid where

import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.LayoutGrid

layoutGrid :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
layoutGrid = HH.div [ HP.class_ mdc_layout_grid ]

cell :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
cell = HH.div [ HP.class_ mdc_layout_grid__cell ]

inner :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
inner = HH.div [ HP.class_ mdc_layout_grid__inner ]
