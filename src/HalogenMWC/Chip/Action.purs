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



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



data Chip r i =
    Material.Chip.Action.Internal.Chip r i



chip :: Config r i -> String -> Chip r i
chip =
    Chip
