module HalogenMWC.Chip.Action.Internal (Chip(..), Config(..)) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA


type Config r i
    =
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }


data Chip r i
    = Chip (Config r i) String
