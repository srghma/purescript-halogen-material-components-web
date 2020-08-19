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



setLeadingIcon :: Maybe String -> Config r i -> Config r i
setLeadingIcon leadingIcon (Config config_) =
    Config { config_ | leadingIcon = leadingIcon }



setTrailingIcon :: Maybe String -> Config r i -> Config r i
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnDelete :: r i -> Config r i -> Config r i
setOnDelete onDelete (Config config_) =
    Config { config_ | onDelete = Just onDelete }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



data Chip r i =
    Material.Chip.Input.Internal.Chip r i



chip :: Config r i -> String -> Chip r i
chip =
    Chip
