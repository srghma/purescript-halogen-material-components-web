module HalogenMWC.Chip.Filter.Internal (Chip(..), Config(..)) where

import Html


type Config r i
    =
        { icon :: Maybe String
        , selected :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe r i
        }


data Chip r i
    = Chip (Config r i) String
