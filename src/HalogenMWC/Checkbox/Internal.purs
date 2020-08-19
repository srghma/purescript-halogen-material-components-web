module HalogenMWC.Checkbox.Internal (Config(..), State(..))

import Html


data Config msg
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
