module HalogenMWC.IconButton.Internal (Config(..))

import Html


type Config r i
    = Config
        { disabled :: Boolean
        , label :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        }
