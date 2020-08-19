module HalogenMWC.Chip.Action.Internal (Chip(..), Config(..)) where

import Html


type Config r i
    =
        { icon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }


data Chip r i
    = Chip (Config r i) String
