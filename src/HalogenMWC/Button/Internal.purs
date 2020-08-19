module HalogenMWC.Button.Internal (Config(..))

import Html


data Config msg
    = Config
        { icon :: Maybe String
        , trailingIcon :: Bool
        , disabled :: Bool
        , dense :: Bool
        , href :: Maybe String
        , target :: Maybe String
        , additionalAttributes :: List (Html.Attribute msg)
        , onClick :: Maybe msg
        , touch :: Bool
        }
