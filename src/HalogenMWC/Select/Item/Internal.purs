module HalogenMWC.Select.Item where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

data Config a r i
    =
        { value :: a
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }

data SelectItem a r i
    = SelectItem (Config a r i) (Array (Html r i))
