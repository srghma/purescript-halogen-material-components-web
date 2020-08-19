module HalogenMWC.Chip.Choice.Internal (Chip(..), Config(..)) where

import Html


type Config r i
    =
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        }


data Chip a r i
    = Chip (Config r i) a
