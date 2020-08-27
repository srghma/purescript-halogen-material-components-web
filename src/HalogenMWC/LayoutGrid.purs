module HalogenMWC.LayoutGrid where

import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | ```
-- | import HalogenMWC.LayoutGrid as LayoutGrid
-- | LayoutGrid.div mdc_layout_grid
-- | ```
div :: forall w i. ClassName -> Array (HH.HTML w i) -> HH.HTML w i
div class_ = HH.div [ HP.class_ class_ ]

-- | ```
-- | import HalogenMWC.LayoutGrid as LayoutGrid
-- | LayoutGrid.div' [ mdc_layout_grid ]
-- | ```
div' :: forall w i. Array ClassName -> Array (HH.HTML w i) -> HH.HTML w i
div' classes = HH.div [ HP.classes classes ]
