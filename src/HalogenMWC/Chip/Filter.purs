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



setSelected :: Boolean -> Config r i -> Config r i
setSelected selected (Config config_) =
    Config { config_ | selected = selected }



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnChange :: r i -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }



data Chip r i =
    Material.Chip.Filter.Internal.Chip r i



chip :: Config r i -> String -> Chip r i
chip =
    Chip
