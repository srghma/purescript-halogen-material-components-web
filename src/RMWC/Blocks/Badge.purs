module RMWC.Blocks.Badge where

import Protolude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Halogen.HTML.Core (AttrName(..))
import Halogen.SVG.Attributes as SVG.Attributes
import Halogen.SVG.Elements as SVG
import Math as Math
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML (ClassName(..))
import Material.Classes.LinearProgress
import Data.Percent (Percent)
import RMWC.Classes.Badge
import MaterialIconsFont.Classes
import Protolude.Url (Url)
import Protolude.Url as Url

inlineNoContent :: forall w i . HH.HTML w i
inlineNoContent = HH.div
  [ HP.classes
    [ rmwc_badge
    , rmwc_badge____align_inline
    , rmwc_badge____no_content
    ]
  ]
  []

inlineWithContent :: forall w i . Array (HH.HTML w i) -> HH.HTML w i
inlineWithContent = HH.div
  [ HP.classes
    [ rmwc_badge
    , rmwc_badge____align_inline
    ]
  ]
