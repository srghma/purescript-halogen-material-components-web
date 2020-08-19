module HalogenMWC.IconButton.Internal (Config(..))

import Html


data Config msg
    = Config
        { disabled :: Bool
        , label :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        }
