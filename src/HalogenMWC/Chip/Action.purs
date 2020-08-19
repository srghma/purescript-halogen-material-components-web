module HalogenMWC.Chip.Action
    ( Config, config



    , chip, Chip
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Action.Internal (Chip(..), Config(..))



type Config r i =
    Material.Chip.Action.Internal.Config r i



config :: Config r i
config =
    Config
        { icon = Nothing
        , additionalAttributes = []
        , onClick = Nothing
        }


















data Chip r i =
    Material.Chip.Action.Internal.Chip r i



chip :: Config r i -> String -> Chip r i
chip =
    Chip
