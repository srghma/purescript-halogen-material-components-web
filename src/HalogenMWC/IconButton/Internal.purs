module HalogenMWC.IconButton.Internal (Config(..))

import Html


data Config msg
    = Config
        { disabled :: Bool
        , label :: Maybe String
        , additionalAttributes :: List (Html.Attribute msg)
        , onClick :: Maybe msg
        }
