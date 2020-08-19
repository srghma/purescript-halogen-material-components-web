module HalogenMWC.Tab.Internal
    ( Config(..)
    , Content
    , Tab(..)
    ) where

import Html


type Config r i
    = Config
        { active :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe r i
        , content :: Content
        }


data Content =
    { label :: String
    , icon :: Maybe String
    }


data Tab r i
    = Tab (Config r i)
