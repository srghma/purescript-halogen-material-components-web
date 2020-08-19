module HalogenMWC.Checkbox.Internal (Config(..), State(..))

import Html


type Config r i
    = Config
        { state :: Maybe State
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe msg
        , touch :: Boolean
        }


data State
    = Unchecked
    | Checked
    | Indeterminate
