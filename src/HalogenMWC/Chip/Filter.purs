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



type Config r i =
    Material.Chip.Filter.Internal.Config r i



config :: Config r i
config =
    Config
        { selected = False
        , icon = Nothing
        , onChange = Nothing
        , additionalAttributes = []
        }























data Chip r i =
    Material.Chip.Filter.Internal.Chip r i



chip :: Config r i -> String -> Chip r i
chip =
    Chip
