module RMWC.Blocks.Badge where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import RMWC.Classes.Badge (rmwc_badge, rmwc_badge____align_inline, rmwc_badge____no_content)

inlineNoContent :: forall w i. HH.HTML w i
inlineNoContent =
  HH.div
    [ HP.classes
        [ rmwc_badge
        , rmwc_badge____align_inline
        , rmwc_badge____no_content
        ]
    ]
    []

inlineWithContent :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
inlineWithContent =
  HH.div
    [ HP.classes
        [ rmwc_badge
        , rmwc_badge____align_inline
        ]
    ]
