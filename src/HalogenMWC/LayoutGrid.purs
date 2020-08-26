module HalogenMWC.LayoutGrid where

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.LayoutGrid (mdc_layout_grid, mdc_layout_grid__cell, mdc_layout_grid__inner)

layoutGrid :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
layoutGrid = HH.div [ HP.class_ mdc_layout_grid ]

cell :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
cell = HH.div [ HP.class_ mdc_layout_grid__cell ]

inner :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
inner = HH.div [ HP.class_ mdc_layout_grid__inner ]
