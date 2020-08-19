module HalogenMWC.Tab.Internal
    ( Config(..)
    , Content
    , Tab(..)
    ) where

import Html


type Config r i
    = Config
        { active :: Bool
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        , content :: Content
        }


data Content =
    { label :: String
    , icon :: Maybe String
    }


data Tab msg
    = Tab (Config msg)
