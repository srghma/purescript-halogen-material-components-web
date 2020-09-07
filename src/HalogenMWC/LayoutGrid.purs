module HalogenMWC.LayoutGrid where

import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | ```
-- | import HalogenMWC.LayoutGrid as LayoutGrid
-- | LayoutGrid.divClass mdc_layout_grid
-- | ```
divClass :: forall w i. ClassName -> Array (HH.HTML w i) -> HH.HTML w i
divClass class_ = HH.div [ HP.class_ class_ ]

-- | ```
-- | import HalogenMWC.LayoutGrid as LayoutGrid
-- | LayoutGrid.divClasses [ mdc_layout_grid ]
-- | ```
divClasses :: forall w i. Array ClassName -> Array (HH.HTML w i) -> HH.HTML w i
divClasses classes = HH.div [ HP.classes classes ]
