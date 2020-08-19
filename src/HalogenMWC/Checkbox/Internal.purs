module HalogenMWC.Checkbox.Internal (Config(..), State(..)) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA


type Config r i
    =
        { state :: Maybe State
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe r i
        , touch :: Boolean
        }


data State
    = Unchecked
    | Checked
    | Indeterminate
