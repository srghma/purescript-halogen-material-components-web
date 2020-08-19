module HalogenMWC.Button.Internal (Config(..)) where

import Html


type Config r i
    = Config
        { icon :: Maybe String
        , trailingIcon :: Boolean
        , disabled :: Boolean
        , dense :: Boolean
        , href :: Maybe String
        , target :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        , touch :: Boolean
        }
