module HalogenMWC.Chip.Input
    ( Config, config





    , chip, Chip
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Input.Internal (Chip(..), Config(..))



type Config r i =
    Material.Chip.Input.Internal.Config r i



config :: Config r i
config =
    Config
        { leadingIcon = Nothing
        , trailingIcon = Nothing
        , additionalAttributes = []
        , onDelete = Nothing
        , onClick = Nothing
        }




























data Chip r i =
    Material.Chip.Input.Internal.Chip r i



chip :: Config r i -> String -> Chip r i
chip =
    Chip
