module HalogenMWC.Chip.Input.Internal (Chip(..), Config(..)) where

import Html


type Config r i
    =
        { leadingIcon :: Maybe String
        , trailingIcon :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        , onDelete :: Maybe r i
        }


data Chip r i
    = Chip (Config r i) String
