module HalogenMWC.Checkbox.Internal (Config(..), State(..))

import Html


type Config r i
    = Config
        { state :: Maybe State
        , disabled :: Bool
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe msg
        , touch :: Bool
        }


data State
    = Unchecked
    | Checked
    | Indeterminate
