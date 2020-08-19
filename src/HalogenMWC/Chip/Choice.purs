module HalogenMWC.Chip.Choice
    ( Config, config
    , chip, Chip
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Choice.Internal (Chip(..), Config(..))



type Config r i =
    Material.Chip.Choice.Internal.Config r i



config :: Config r i
config =
    Config
        { icon = Nothing
        , additionalAttributes = []
        }



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



data Chip a r i =
    Material.Chip.Choice.Internal.Chip a r i



chip :: Config r i -> a -> Chip a r i
chip =
    Chip
