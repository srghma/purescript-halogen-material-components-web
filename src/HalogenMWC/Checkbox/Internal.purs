module HalogenMWC.Checkbox.Internal (Config(..), State(..)) where

import Html


type Config r i
    = Config
        { state :: Maybe State
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe r i
        , touch :: Boolean
        }


data State
    = Unchecked
    | Checked
    | Indeterminate
