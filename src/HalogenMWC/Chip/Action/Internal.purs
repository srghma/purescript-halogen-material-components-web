module HalogenMWC.Chip.Action.Internal (Chip(..), Config(..))

import Html


type Config r i
    = Config
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }


data Chip r i
    = Chip (Config r i) String
