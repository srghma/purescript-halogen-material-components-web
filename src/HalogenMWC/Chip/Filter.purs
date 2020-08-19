module HalogenMWC.Chip.Filter
    ( Config, config

    , chip, Chip
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Filter.Internal (Chip(..), Config(..))

type Config r i
  = { icon :: Maybe String
    , selected :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onChange :: Maybe r i
    }

data Chip r i
  = Chip (Config r i) String

config :: Config r i
config =
    Config
        { selected = False
        , icon = Nothing
        , onChange = Nothing
        , additionalAttributes = []
        }
